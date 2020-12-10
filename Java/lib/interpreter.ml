open Ast
open Parser
open Hashtbl

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val error : string -> 'a t
end

module Result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind

  let return = Result.ok

  let error = Result.error
end

type key_t = string [@@deriving show]

type constructor_r = { args : (type_t * name) list; body : stmt }
[@@deriving show { with_path = false }]

type field_r = {
  f_type : type_t;
  key : key_t;
  is_mutable : bool;
  mutable f_value : value option;
  sub_tree : expr option;
}
[@@deriving show { with_path = false }]

type method_r = {
  m_type : type_t;
  is_abstract : bool;
  is_overridable : bool;
  has_override_annotation : bool;
  args : (type_t * name) list;
  key : key_t;
  body : stmt option;
}
[@@deriving show { with_path = false }]

type class_r = {
  this_key : key_t;
  mutable field_table : (key_t, field_r) Hashtbl.t;
  mutable method_table : (key_t, method_r) Hashtbl.t;
  mutable constructor_table : (key_t, constructor_r) Hashtbl.t;
  mutable children_keys : key_t list;
  is_abstract : bool;
  is_inheritable : bool;
  parent_key : key_t option;
}

let class_table : (key_t, class_r) Hashtbl.t = Hashtbl.create 1024

let convert_name_to_key = function Some (Name x) -> Some x | None -> None

exception InheritanceException of string

module ClassLoader (M : MONADERROR) = struct
  open M

  let is_abstract = List.mem Abstract

  let is_final = List.mem Final

  let is_override = List.mem Override

  let is_static = List.mem Static

  let is_public = List.mem Public

  (*Функция для проверки полей, методов и конструкторов на наличие бредовых модификаторов*)
  let check_modifiers_f pair =
    match pair with
    | l, f -> (
        match f with
        (*public static void main (String[] args)*)
        | Method (Void, Name "main", [ (Array String, Name "args") ], _) ->
            if
              is_static l && is_public l
              && (not (is_abstract l))
              && (not (is_final l))
              && not (is_override l)
            then return ()
            else error "Wrong method modifiers"
        (*Простые методы - не статичные, не могут быть абстрактными и финальными одновременно*)
        | Method (_, Name n, _, _) ->
            if is_abstract l && is_final l then error "Wrong method modifiers"
            else if is_static l && n <> "main" then
              error "Wrong method modifiers"
            else return ()
        (*Поля - не статичные, не абстрактные, не override*)
        | VarField (_, _) ->
            if
              (not (is_static l))
              && (not (is_abstract l))
              && not (is_override l)
            then return ()
            else error "Wrong field modifiers"
        (*Конструкторы - могут быть либо публичными, либо дефолтными*)
        | Constructor (_, _, _) ->
            if
              (not (is_static l))
              && (not (is_abstract l))
              && (not (is_final l))
              && not (is_override l)
            then return ()
            else error "Wrong constructor modifiers" )

  (*Функция для проверки класса на наличие бредовых модификаторов*)
  let check_modifiers_c = function
    | Class (ml, _, _, _) ->
        if is_abstract ml && is_final ml then error "Wrong class modifiers"
        else if (not (is_static ml)) && not (is_override ml) then return ()
        else error "Wrong class modifiers"

  let get_type_list = List.map (function t, _ -> t)

  let get_class_by_key key = Hashtbl.find class_table key

  (* Отдельная функция для добавления в таблицу с проверкой на существование *)
  let add_with_check ht key value e_message =
    match Hashtbl.find_all ht key with
    | [] -> return (Hashtbl.add ht key value)
    | _ -> error e_message

  (* Сначала надо просто заполнить таблицу классов *)
  let c_table_add : class_dec list -> unit M.t =
   fun cd_list ->
    (* По class_dec получить class_r и добавить его в таблицу *)
    let add_to_class_table : class_dec -> unit M.t =
     fun class_d ->
      match class_d with
      | Class (ml, Name cl_n, parent_o, fields) ->
          (* У нас не может быть несколько одинаковых классов в программе *)
          let check_new =
            match Hashtbl.find_all class_table cl_n with
            | [] -> return ()
            | _ -> error "Similar classes"
          in
          (* Инициализируем таблицы *)
          let m_table = Hashtbl.create 1024 in
          let f_table = Hashtbl.create 1024 in
          let c_table = Hashtbl.create 1024 in
          check_new >>= fun _ ->
          check_modifiers_c class_d >>= fun _ ->
          (* Функция добавления элемента класса в соответствующую таблицу *)
          let add_field : modifier list * field -> unit M.t =
           fun field_elem ->
            match field_elem with
            | f_ms, VarField (f_t, pairs) ->
                let rec helper = function
                  | [] -> return ()
                  | (Name f_name, expr_o) :: ps ->
                      (* В качестве ключа выступает имя поля *)
                      add_with_check f_table f_name
                        {
                          f_type = f_t;
                          key = f_name;
                          is_mutable = is_final f_ms;
                          f_value = None;
                          sub_tree = expr_o;
                        }
                        "Similar fields"
                      >>= fun _ -> helper ps
                in
                check_modifiers_f field_elem >>= fun _ -> helper pairs
            | m_ms, Method (m_t, Name name, args_list, m_body) ->
                (* Формирование ключа: method_key = name ++ type1 ++ type2 ++ ... ++ typen *)
                let method_key =
                  String.concat ""
                    (name :: List.map show_type_t (get_type_list args_list))
                in
                let is_class_abstract = is_abstract ml in
                let is_method_abstract = is_abstract m_ms in

                (*Перед добавлением стоит проверять, чтобы у абстрактного класса не было тела и прочие ошибки*)
                let check_abstract_body_syntax =
                  match is_method_abstract with
                  | true -> (
                      if not is_class_abstract then
                        error "Abstract method in non-abstract class"
                      else
                        match m_body with
                        | Some _ -> error "Abstract method cannot have body"
                        | None -> return () )
                  | false -> (
                      match m_body with
                      | Some _ -> return ()
                      | None -> error "No body of non-abstract method" )
                in
                check_modifiers_f field_elem >>= fun _ ->
                check_abstract_body_syntax >>= fun _ ->
                add_with_check m_table method_key
                  {
                    m_type = m_t;
                    is_abstract = is_method_abstract;
                    is_overridable = not (is_final m_ms);
                    has_override_annotation = is_override m_ms;
                    args = args_list;
                    key = method_key;
                    body = m_body;
                  }
                  "Method with this type exists"
            | _, Constructor (Name name, args_list, c_body) ->
                let constr_key =
                  String.concat ""
                    (name :: List.map show_type_t (get_type_list args_list))
                in
                (*Смотрим, чтобы имя конструктора совпадало с классом*)
                let check_names_match =
                  if name = cl_n then return ()
                  else error "Constructor name error"
                in
                check_names_match >>= fun _ ->
                check_modifiers_f field_elem >>= fun _ ->
                add_with_check c_table constr_key
                  { args = args_list; body = c_body }
                  "Constructor with this type exists"
          in
          let rec helper_add = function
            | [] -> return ()
            | f :: fs -> add_field f >>= fun _ -> helper_add fs
          in
          helper_add fields >>= fun _ ->
          return
            (Hashtbl.add class_table cl_n
               {
                 this_key = cl_n;
                 field_table = f_table;
                 method_table = m_table;
                 constructor_table = c_table;
                 children_keys = [];
                 is_abstract = is_abstract ml;
                 is_inheritable = not (is_final ml);
                 parent_key = convert_name_to_key parent_o;
               })
    in
    let rec helper_classes_add = function
      | [] -> return ()
      | c :: cs -> add_to_class_table c >>= fun _ -> helper_classes_add cs
    in
    helper_classes_add cd_list

  (* После добавления надо обновить child_keys у каждого класса *)
  let update_child_keys_exn : unit M.t =
    let update key = function
      | {
          this_key = _;
          field_table = _;
          method_table = _;
          constructor_table = _;
          children_keys = _;
          is_abstract = _;
          is_inheritable = _;
          parent_key = p_key_o;
        } -> (
          match p_key_o with
          | None -> ()
          | Some p_key ->
              let parent =
                try get_class_by_key p_key
                with Not_found ->
                  raise (InheritanceException "No parent class found")
              in
              let par_ch_keys = parent.children_keys in
              if parent.is_inheritable then
                (* От final класса наследоваться нельзя, проверяем *)
                parent.children_keys <- key :: par_ch_keys
              else
                raise (InheritanceException "Final class cannot be inherited") )
    in
    return (Hashtbl.iter update class_table)

  (* Отдельная функция, которая берет родителя и ребенка,
        свойства родителя передает ребенку с необходимыми проверками, далее обрабатывает рекурсивно все дерево наследования от родителя *)
  let rec transfert : class_r -> class_r -> unit =
   fun parent child ->
    (* Обработка поля родителя *)
    let process_field : key_t -> field_r -> unit =
     fun _ cur_field ->
      (* Смотрим, есть ли такое поле в таблице ребенка*)
      match Hashtbl.find_all child.field_table cur_field.key with
      (* Нет - просто добавляем в таблицу ребенка *)
      | [] -> Hashtbl.add child.field_table cur_field.key cur_field
      (* Есть - ну и ладно, пропускаем *)
      | _ -> ()
    in
    let process_fields = Hashtbl.iter process_field parent.field_table in
    (* Конструкторы не переносим, но у конструкторов ребенка первый стейтмент - вызов super(...). Проверяем это *)
    let check_child_constructors_exn =
      let check_constructor_exn : key_t -> constructor_r -> unit =
       fun _ cur_cr ->
        match cur_cr with
        | {
         args = _;
         body = StmtBlock (Expression (CallMethod (Super, _)) :: _);
        } ->
            ()
        | _ ->
            raise
              (InheritanceException
                 "No super headed statement in inherited constructor")
      in
      if Hashtbl.length parent.constructor_table > 0 then
        Hashtbl.iter check_constructor_exn child.constructor_table
    in
    (* Перенос метода. Тут надо много всего проверять на абстрактность *)
    let process_method_exn : key_t -> method_r -> unit =
     fun _ cur_method ->
      match Hashtbl.find_all child.method_table cur_method.key with
      | [] -> (
          (* Не нашли переопределенного метода - смотрим, наш абстрактный? *)
          match cur_method.is_abstract with
          | true ->
              (* Наш абстрактный. Если ребенок абстрактный, то просто переносим метод, иначе бросаем исключение *)
              if child.is_abstract then
                Hashtbl.add child.method_table cur_method.key cur_method
              else
                raise (InheritanceException "Abstract method must be overriden")
          | false ->
              if cur_method.is_overridable then
                Hashtbl.add child.method_table cur_method.key cur_method
                (*Наш не абстрактный. Если наш не final - переносим в ребенка *)
          )
      (* Нашли переопредленный метод - ну и ок, проверять тут нечего *)
      | _ -> ()
    in

    let process_methods_exn =
      Hashtbl.iter process_method_exn parent.method_table
    in
    (* Проверка на то, что аннотации @Override стоят только у переопределенных методов*)
    let check_override_annotations_exn =
      let check_override_ann : key_t -> method_r -> unit =
       fun _ ch_mr ->
        match
          ch_mr.has_override_annotation
          && Hashtbl.find_all parent.method_table ch_mr.key = []
        with
        | true -> ()
        | false ->
            raise
              (InheritanceException
                 "@Override annotation on not overriden method")
      in
      Hashtbl.iter check_override_ann child.method_table
    in
    let run_transfert_on_children =
      let process_child ch_key =
        (*Нашли ребенка - он есть, конечно*)
        let child_by_key = get_class_by_key ch_key in
        (* Производим запуск transfert между child_by_key и каждым ребенком child_by_key *)
        List.iter
          (fun ch_ch_key -> transfert child_by_key (get_class_by_key ch_ch_key))
          child_by_key.children_keys
      in
      List.iter process_child child.children_keys
    in
    process_fields |> fun _ ->
    process_methods_exn |> fun _ ->
    check_override_annotations_exn |> fun _ ->
    check_child_constructors_exn |> fun _ -> run_transfert_on_children

  let do_inheritance =
    let processing _ cur_class =
      match cur_class with
      | {
       this_key = _;
       field_table = _;
       method_table = _;
       constructor_table = _;
       children_keys = ch_list;
       is_abstract = _;
       is_inheritable = _;
       parent_key = p_key_o;
      } -> (
          match p_key_o with
          | Some _ -> ()
          | None ->
              List.iter
                (fun c_key -> transfert cur_class (get_class_by_key c_key))
                ch_list )
    in
    return (Hashtbl.iter processing class_table)

  let load cd_list =
    c_table_add cd_list >>= fun _ ->
    ( try update_child_keys_exn
      with InheritanceException message -> error message )
    >>= fun _ ->
    try do_inheritance with InheritanceException message -> error message
end
