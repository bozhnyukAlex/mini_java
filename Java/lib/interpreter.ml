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
  field_table : (key_t, field_r) Hashtbl.t;
  method_table : (key_t, method_r) Hashtbl.t;
  constructor_table : (key_t, constructor_r) Hashtbl.t;
  mutable children_keys : key_t list;
  is_abstract : bool;
  is_inheritable : bool;
  parent_key : key_t option;
}

let class_table : (key_t, class_r) Hashtbl.t = Hashtbl.create 1024

let convert_name_to_key = function Some (Name x) -> Some x | None -> None

(* Не уверен, что стоит жрать память для списка, если там есть Hashtbl.to_seq_values. Потом исправите, если что *)
let convert_table_to_list ht = Hashtbl.fold (fun _ v acc -> v :: acc) ht []

module ClassLoader (M : MONADERROR) = struct
  open M

  let is_abstract = List.mem Abstract

  let is_final = List.mem Final

  let is_override = List.mem Override

  let is_static = List.mem Static

  let is_public = List.mem Public

  let monadic_update_hash_table ht old_key new_val =
    return (Hashtbl.replace ht old_key new_val)

  let rec monadic_list_iter list action =
    match list with
    | [] -> return ()
    | x :: xs -> action x >>= fun _ -> monadic_list_iter xs action

  let prepare_object ht =
    let constructor_table = Hashtbl.create 20 in
    let field_table = Hashtbl.create 20 in
    let method_table = Hashtbl.create 20 in
    let equals : method_r =
      {
        m_type = Int;
        is_abstract = false;
        is_overridable = true;
        has_override_annotation = false;
        args = [ (ClassName "Object", Name "obj") ];
        key = "equals@@";
        body =
          apply Stmt.stat_block
            {| 
          {
              if (this == obj) return 1;
              else return 0;
          }
    |};
      }
    in
    let to_string : method_r =
      {
        m_type = String;
        is_abstract = false;
        is_overridable = true;
        has_override_annotation = false;
        args = [];
        key = "toString@@";
        body =
          apply Stmt.stat_block
            {|
          {
            return "Object";
          }
        |};
      }
    in
    return
      ( Hashtbl.add method_table "equals@@" equals;
        Hashtbl.add method_table "toString@@" to_string;
        Hashtbl.add ht "Object"
          {
            this_key = "Object";
            field_table;
            method_table;
            constructor_table;
            children_keys = [];
            is_abstract = false;
            is_inheritable = true;
            parent_key = None;
          } )

  (*Функция для проверки полей, методов и конструкторов на наличие бредовых модификаторов*)
  let check_modifiers_f pair =
    match pair with
    | l, f -> (
        match f with
        (*public static void main (String[] args)*)
        | Method (Void, Name "main", [ (Array String, Name "args") ], _) -> (
            match
              ( is_static l,
                is_public l,
                is_abstract l,
                is_final l,
                is_override l )
            with
            | true, true, false, false, false -> return ()
            | _ -> error "Wrong method modifiers" )
        (*Простые методы - не статичные, не могут быть абстрактными и финальными одновременно*)
        | Method (_, _, _, _) -> (
            match (is_abstract l, is_final l, is_static l) with
            | true, true, _ -> error "Wrong method modifiers"
            | _, _, true -> error "Wrong method modifiers"
            | _ -> return () )
        (*Поля - не статичные, не абстрактные, не override*)
        | VarField (_, _) -> (
            match (is_static l, is_abstract l, is_override l) with
            | false, false, false -> return ()
            | _ -> error "Wrong field modifiers" )
        (*Конструкторы - могут быть либо публичными, либо дефолтными*)
        | Constructor (_, _, _) -> (
            match (is_static l, is_abstract l, is_final l, is_override l) with
            | false, false, false, false -> return ()
            | _ -> error "Wrong constructor modifiers" ) )

  (*Функция для проверки класса на наличие бредовых модификаторов*)
  let check_modifiers_c = function
    | Class (ml, _, _, _) -> (
        match (is_abstract ml, is_final ml, is_static ml, is_override ml) with
        | true, true, _, _ -> error "Wrong class modifiers"
        | _, _, false, false -> return ()
        | _ -> error "Wrong class modifiers" )

  let get_type_list = List.map fst

  let get_elem_if_present ht key = Hashtbl.find_opt ht key

  (* Отдельная функция для добавления в таблицу с проверкой на существование *)
  let add_with_check ht key value e_message =
    match get_elem_if_present ht key with
    | None -> return (Hashtbl.add ht key value)
    | _ -> error e_message

  (* Сначала надо просто заполнить таблицу классов *)
  let c_table_add : class_dec list -> unit M.t =
   fun cd_list ->
    (* По class_dec получить class_r и добавить его в таблицу *)
    let add_to_class_table : class_dec -> unit M.t =
     fun class_d ->
      match class_d with
      | Class (ml, Name this_key, parent_o, fields) ->
          (* Инициализируем таблицы *)
          let method_table = Hashtbl.create 1024 in
          let field_table = Hashtbl.create 1024 in
          let constructor_table = Hashtbl.create 1024 in
          check_modifiers_c class_d >>= fun _ ->
          (* Функция добавления элемента класса в соответствующую таблицу *)
          let add_field : modifier list * field -> unit M.t =
           fun field_elem ->
            match field_elem with
            | f_ms, VarField (f_type, pairs) ->
                let rec helper = function
                  | [] -> return ()
                  | (Name key, sub_tree) :: ps ->
                      let is_mutable = is_final f_ms in
                      let f_value = None in
                      (* В качестве ключа выступает имя поля *)
                      add_with_check field_table key
                        { f_type; key; is_mutable; f_value; sub_tree }
                        "Similar fields"
                      >>= fun _ -> helper ps
                in
                check_modifiers_f field_elem >>= fun _ -> helper pairs
            | m_ms, Method (m_type, Name name, args, body) ->
                (* Формирование ключа: method_key = name ++ type1 ++ type2 ++ ... ++ typen *)
                let key =
                  String.concat ""
                    (name :: List.map show_type_t (get_type_list args))
                  ^ "@@"
                in
                let is_class_abstract = is_abstract ml in
                (* Является ли метод абстрактным *)
                let is_abstract = is_abstract m_ms in

                (*Перед добавлением стоит проверять, чтобы у абстрактного класса не было тела и прочие ошибки*)
                let check_abstract_body_syntax =
                  match is_abstract with
                  | true -> (
                      if not is_class_abstract then
                        error "Abstract method in non-abstract class"
                      else
                        match body with
                        | Some _ -> error "Abstract method cannot have body"
                        | None -> return () )
                  | false -> (
                      match body with
                      | Some _ -> return ()
                      | None -> error "No body of non-abstract method" )
                in
                let is_overridable = not (is_final m_ms) in
                let has_override_annotation = is_override m_ms in
                check_modifiers_f field_elem >>= fun _ ->
                check_abstract_body_syntax >>= fun _ ->
                add_with_check method_table key
                  {
                    m_type;
                    is_abstract;
                    is_overridable;
                    has_override_annotation;
                    args;
                    key;
                    body;
                  }
                  "Method with this type exists"
            | _, Constructor (Name name, args, body) ->
                let constr_key =
                  String.concat ""
                    (name :: List.map show_type_t (get_type_list args))
                  ^ "$$"
                in
                (*Смотрим, чтобы имя конструктора совпадало с классом*)
                let check_names_match =
                  if name = this_key then return ()
                  else error "Constructor name error"
                in
                check_names_match >>= fun _ ->
                check_modifiers_f field_elem >>= fun _ ->
                add_with_check constructor_table constr_key { args; body }
                  "Constructor with this type exists"
          in
          let add_parent p =
            match p with None -> Some "Object" | _ -> convert_name_to_key p
          in
          let children_keys = [] in
          let is_abstract = is_abstract ml in
          let is_inheritable = not (is_final ml) in
          let parent_key = add_parent parent_o in
          monadic_list_iter fields add_field >>= fun _ ->
          add_with_check class_table this_key
            {
              this_key;
              field_table;
              method_table;
              constructor_table;
              children_keys;
              is_abstract;
              is_inheritable;
              parent_key;
            }
            "Similar Classes"
    in
    monadic_list_iter cd_list add_to_class_table

  (* После добавления надо обновить child_keys у каждого класса *)
  let update_child_keys ht =
    (* cr - потенциальный ребенок *)
    let update cr =
      match cr.parent_key with
      (* Ключа родителя нет - идем дальше *)
      | None -> return ()
      (* Есть - пытаемся получить родителя по ключу (или грохаемся с ошибкой), если можно наследоваться - обновляем хеш-таблицу *)
      | Some p_key -> (
          let parent_o = get_elem_if_present ht p_key in
          match parent_o with
          | None -> error "No parent class found"
          | Some parent ->
              if parent.is_inheritable then
                monadic_update_hash_table ht p_key
                  {
                    parent with
                    children_keys = parent.children_keys @ [ cr.this_key ];
                  }
              else error "Final class cannot be inherited" )
    in
    monadic_list_iter (convert_table_to_list ht) update

  (* Отдельная функция, которая берет родителя и ребенка,
        свойства родителя передает ребенку с необходимыми проверками, далее обрабатывает рекурсивно все дерево наследования от родителя *)
  let rec transfert : class_r -> class_r -> unit t =
   fun parent child ->
    (* Обработка поля родителя *)
    let process_field : class_r -> field_r -> unit t =
     fun ch cur_field ->
      (* Смотрим, есть ли такое поле в таблице ребенка*)
      match get_elem_if_present ch.field_table cur_field.key with
      (* Нет - просто добавляем в таблицу ребенка *)
      | None -> return (Hashtbl.add ch.field_table cur_field.key cur_field)
      (* Есть - ну и ладно, пропускаем *)
      | _ -> return ()
    in
    (* let process_fields = Hashtbl.iter process_field parent.field_table in *)
    let process_fields par ch =
      monadic_list_iter
        (convert_table_to_list par.field_table)
        (process_field ch)
    in
    (* Конструкторы не переносим, но у конструкторов ребенка первый стейтмент - вызов super(...). Проверяем это *)
    let check_child_constructors par ch =
      let check_constructor : constructor_r -> unit t = function
        | {
            args = _;
            body = StmtBlock (Expression (CallMethod (Super, _)) :: _);
          } ->
            return ()
        | _ -> error "No super headed statement in inherited constructor"
      in
      if Hashtbl.length par.constructor_table > 0 then
        monadic_list_iter
          (convert_table_to_list ch.constructor_table)
          check_constructor
      else return ()
    in
    (* Перенос метода. Тут надо много всего проверять на абстрактность *)
    let process_method : class_r -> method_r -> unit t =
     fun ch cur_method ->
      match get_elem_if_present ch.method_table cur_method.key with
      | None -> (
          (* Не нашли переопределенного метода - смотрим, наш абстрактный? *)
          match cur_method.is_abstract with
          | true ->
              (* Наш абстрактный. Если ребенок абстрактный, то просто переносим метод, иначе бросаем исключение *)
              if ch.is_abstract then
                return (Hashtbl.add ch.method_table cur_method.key cur_method)
              else error "Abstract method must be overriden"
          | false ->
              if cur_method.is_overridable then
                return (Hashtbl.add ch.method_table cur_method.key cur_method)
                (*Наш не абстрактный. Если наш не final - переносим в ребенка *)
              else return () )
      (* Нашли переопредленный метод - ну и ок, проверять тут нечего *)
      | _ -> return ()
    in

    let process_methods par ch =
      monadic_list_iter
        (convert_table_to_list par.method_table)
        (process_method ch)
    in
    (* Проверка на то, что аннотации @Override у ребенка стоят только у переопределенных методов*)
    let check_override_annotations par ch =
      let check_override_ann : class_r -> method_r -> unit t =
       fun par ch_mr ->
        match ch_mr.has_override_annotation with
        (* Нет аннотации - пропускаем *)
        | false -> return ()
        (* Есть - смотрим, есть ли такой метод в родителе, если есть - все ок, если нет - ошибка*)
        | true -> (
            match get_elem_if_present par.method_table ch_mr.key with
            | None -> error "@Override annotation on not overriden method"
            | _ -> return () )
      in
      (* Hashtbl.iter check_override_ann child.method_table *)
      monadic_list_iter
        (convert_table_to_list ch.method_table)
        (check_override_ann par)
    in
    (* Запуск transfert на ребенке и детях ребенка *)
    let run_transfert_on_children ht ch =
      let childs_of_child = ch.children_keys in
      monadic_list_iter childs_of_child (fun ch_ch_key ->
          transfert ch (Option.get (get_elem_if_present ht ch_ch_key)))
    in
    process_fields parent child >>= fun _ ->
    process_methods parent child >>= fun _ ->
    check_override_annotations parent child >>= fun _ ->
    check_child_constructors parent child >>= fun _ ->
    run_transfert_on_children class_table child

  let do_inheritance ht =
    let obj_r = Option.get (get_elem_if_present ht "Object") in
    let processing ch_key =
      transfert obj_r (Option.get (get_elem_if_present ht ch_key))
    in
    monadic_list_iter obj_r.children_keys processing

  let load cd_list =
    match cd_list with
    | [] -> error "Syntax error or empty file"
    | _ ->
        prepare_object class_table >>= fun _ ->
        c_table_add cd_list >>= fun _ ->
        update_child_keys class_table >>= fun _ -> do_inheritance class_table
end
