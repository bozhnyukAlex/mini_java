open Ast
open Parser
open Hashtbl

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( >> ) : 'a t -> 'b t -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val get : 'a t -> 'a

  val error : string -> 'a t
end

module Result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind

  let return = Result.ok

  let error = Result.error

  let ( >> ) x f = x >>= fun _ -> f

  let get = Result.get_ok
end

type key_t = string [@@deriving show]

type constructor_r = { args : (type_t * name) list; body : stmt }
[@@deriving show { with_path = false }]

type field_r = {
  f_type : type_t;
  key : key_t;
  is_mutable : bool;
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
  children_keys : key_t list;
  is_abstract : bool;
  is_inheritable : bool;
  parent_key : key_t option;
}

let class_table : (key_t, class_r) Hashtbl.t = Hashtbl.create 1024

let convert_name_to_key = function Some (Name x) -> Some x | None -> None

(* TODO: Не уверен, что стоит жрать память для списка, если там есть Hashtbl.to_seq_values. Потом исправите, если что *)
let convert_table_to_list ht = Hashtbl.fold (fun _ v acc -> v :: acc) ht []

let get_elem_if_present ht key = Hashtbl.find_opt ht key

module ClassLoader (M : MONADERROR) = struct
  open M

  let is_abstract = List.mem Abstract

  let is_final = List.mem Final

  let is_override = List.mem Override

  let is_static = List.mem Static

  let is_public = List.mem Public

  let monadic_update_hash_table ht old_key new_val =
    return
      ( Hashtbl.replace ht old_key new_val;
        ht )

  let rec monadic_list_iter list action base =
    match list with
    | [] -> return base
    | x :: xs -> action x >> monadic_list_iter xs action base

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
          };
        ht )

  (*Функция для проверки полей, методов и конструкторов на наличие бредовых модификаторов*)

  (*Функция для проверки полей, методов и конструкторов на наличие бредовых модификаторов*)
  let check_modifiers_f pair =
    match pair with
    | l, f -> (
        match f with
        (*public static void main ()*)
        | Method (Void, Name "main", [], _)
          when is_static l && is_public l
               && (not (is_abstract l))
               && (not (is_final l))
               && not (is_override l) ->
            return ()
        | Method (_, Name "main", _, _) ->
            error "Only one main method can be in program!"
        (*Простые методы - не статичные, не могут быть абстрактными и финальными одновременно*)
        | Method (_, _, _, _) when is_abstract l && is_final l ->
            error "Wrong method modifiers"
        | Method (_, _, _, _) when is_static l -> error "Wrong method modifiers"
        | Method (_, _, _, _) -> return ()
        (*Поля - не статичные, не абстрактные, не override*)
        | VarField (_, _)
          when (not (is_static l))
               && (not (is_abstract l))
               && not (is_override l) ->
            return ()
        | VarField (_, _) -> error "Wrong field modifiers"
        (*Конструкторы - могут быть либо публичными, либо дефолтными*)
        | Constructor (_, _, _)
          when (not (is_static l))
               && (not (is_abstract l))
               && (not (is_final l))
               && not (is_override l) ->
            return ()
        | Constructor (_, _, _) -> error "Wrong constructor modifiers" )

  (*Функция для проверки класса на наличие бредовых модификаторов*)
  let check_modifiers_c = function
    | Class (ml, _, _, _) when is_abstract ml && is_final ml ->
        error "Wrong class modifiers"
    | Class (ml, _, _, _) when (not (is_static ml)) && not (is_override ml) ->
        return ()
    | Class (_, _, _, _) -> error "Wrong class modifiers"

  let get_type_list = List.map fst

  (* Отдельная функция для добавления в таблицу с проверкой на существование *)
  let add_with_check ht key value e_message =
    match get_elem_if_present ht key with
    | None ->
        return
          ( Hashtbl.add ht key value;
            ht )
    | _ -> error e_message

  (* Сначала надо просто заполнить таблицу классов *)
  let add_to_class_table class_ht class_d =
    match class_d with
    | Class (ml, Name this_key, parent_o, fields) ->
        (* Инициализируем таблицы *)
        let method_table = Hashtbl.create 1024 in
        let field_table = Hashtbl.create 1024 in
        let constructor_table = Hashtbl.create 1024 in
        check_modifiers_c class_d
        >>
        (* Функция добавления элемента класса в соответствующую таблицу *)
        let add_field : modifier list * field -> unit M.t =
         fun field_elem ->
          match field_elem with
          | f_ms, VarField (f_type, pairs) ->
              let rec helper = function
                | [] -> return ()
                | (Name key, sub_tree) :: ps ->
                    let is_mutable = is_final f_ms in
                    (* В качестве ключа выступает имя поля *)
                    add_with_check field_table key
                      { f_type; key; is_mutable; sub_tree }
                      "Similar fields"
                    >> helper ps
              in
              check_modifiers_f field_elem >> helper pairs
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
              check_modifiers_f field_elem
              >> check_abstract_body_syntax
              >> add_with_check method_table key
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
              >> return ()
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
              check_names_match
              >> check_modifiers_f field_elem
              >> add_with_check constructor_table constr_key { args; body }
                   "Constructor with this type exists"
              >> return ()
        in
        let add_parent p =
          match p with None -> Some "Object" | _ -> convert_name_to_key p
        in
        let is_abstract = is_abstract ml in
        let is_inheritable = not (is_final ml) in
        let parent_key = add_parent parent_o in
        monadic_list_iter fields add_field ()
        >> add_with_check class_ht this_key
             {
               this_key;
               field_table;
               method_table;
               constructor_table;
               children_keys = [];
               is_abstract;
               is_inheritable;
               parent_key;
             }
             "Similar Classes"

  let c_table_add cd_list cl_ht =
    monadic_list_iter cd_list (add_to_class_table cl_ht) cl_ht

  let update_child_keys ht =
    let update : class_r -> class_r M.t =
     fun cr ->
      match cr.parent_key with
      (* Ключа родителя нет - идем дальше *)
      | None -> return cr
      (* Есть - пытаемся получить родителя по ключу (или грохаемся с ошибкой), если можно наследоваться - обновляем хеш-таблицу *)
      | Some p_key -> (
          let parent_o = get_elem_if_present ht p_key in
          match parent_o with
          | None -> error "No parent class found"
          | Some parent when parent.is_inheritable ->
              let new_val =
                {
                  parent with
                  children_keys = cr.this_key :: parent.children_keys;
                }
              in
              monadic_update_hash_table ht p_key new_val >> return new_val
          | Some _ -> error "Final class cannot be inherited" )
    in
    monadic_list_iter (convert_table_to_list ht) update ht

  (* Мелкие функции по обработке отдельных частей для transfert *)

  (* Обработка поля родителя *)
  let process_field : class_r -> field_r -> unit t =
   fun ch cur_field ->
    (* Смотрим, есть ли такое поле в таблице ребенка*)
    match get_elem_if_present ch.field_table cur_field.key with
    (* Нет - просто добавляем в таблицу ребенка *)
    | None -> return (Hashtbl.add ch.field_table cur_field.key cur_field)
    (* Есть - ну и ладно, пропускаем *)
    | _ -> return ()

  let process_fields par ch =
    monadic_list_iter
      (convert_table_to_list par.field_table)
      (process_field ch) ()

  let is_super : stmt -> bool = function
    | Expression (CallMethod (Super, _)) -> true
    | _ -> false

  let body_starts_with_super : constructor_r -> bool = function
    | { args = _; body = StmtBlock (Expression (CallMethod (Super, _)) :: _) }
      ->
        true
    | _ -> false

  (* Конструкторы не переносим, но у конструкторов ребенка первый стейтмент - вызов super(...). Проверяем это *)
  let check_child_constructors par ch =
    let check_constructor : constructor_r -> unit t =
     fun constr_r ->
      match constr_r with
      | { args = _; body = StmtBlock stlist } -> (
          match List.filter is_super stlist with
          | [] -> error "No super statement in inherited constructor"
          | [ _ ] ->
              if body_starts_with_super constr_r then return ()
              else error "Super statement must br first in constructor"
          | _ -> error "Only one super statement must be in constructor" )
      | _ -> error "Constructor body must be in block!"
    in
    if Hashtbl.length par.constructor_table > 0 then
      monadic_list_iter
        (convert_table_to_list ch.constructor_table)
        check_constructor ()
    else return ()

  let is_this : stmt -> bool = function
    | Expression (CallMethod (This, _)) -> true
    | _ -> false

  let body_starts_with_this : constructor_r -> bool = function
    | { args = _; body = StmtBlock (Expression (CallMethod (This, _)) :: _) } ->
        true
    | _ -> false

  (* Надо у текущего класса проверять, чтобы если в его конструкторах есть вызов this(...), то он должен быть первым и единственным *)
  let check_cur_constructors cur =
    let check_constructor : constructor_r -> unit t =
     fun constr_r ->
      match constr_r.body with
      | StmtBlock stlist -> (
          match List.filter is_this stlist with
          | [] -> return ()
          | [ _ ] ->
              if body_starts_with_this constr_r then return ()
              else error "This constructor call must be in the beginning"
          | _ -> error "More then one constructor calls!" )
      | _ -> error "Constructor body must be in block!"
    in
    monadic_list_iter
      (convert_table_to_list cur.constructor_table)
      check_constructor ()

  (* Перенос метода. Тут надо много всего проверять на абстрактность *)
  let process_method : class_r -> method_r -> unit t =
   fun ch cur_method ->
    match get_elem_if_present ch.method_table cur_method.key with
    | None when cur_method.is_abstract ->
        (* Наш абстрактный. Если ребенок абстрактный, то просто переносим метод, иначе бросаем исключение *)
        if ch.is_abstract then
          return (Hashtbl.add ch.method_table cur_method.key cur_method)
        else error "Abstract method must be overriden"
    | None when not cur_method.is_abstract ->
        (*Наш не абстрактный. Если наш не final - переносим в ребенка *)
        if cur_method.is_overridable then
          return (Hashtbl.add ch.method_table cur_method.key cur_method)
        else return ()
    | _ -> return ()

  let process_methods par ch =
    monadic_list_iter
      (convert_table_to_list par.method_table)
      (process_method ch) ()

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
    monadic_list_iter
      (convert_table_to_list ch.method_table)
      (check_override_ann par) ()

  (* Отдельная функция, которая берет родителя и ребенка,
        свойства родителя передает ребенку с необходимыми проверками, далее обрабатывает рекурсивно все дерево наследования от родителя *)
  let rec transfert : class_r -> class_r -> unit t =
   fun parent child ->
    process_fields parent child
    >> process_methods parent child
    >> check_override_annotations parent child
    >> check_cur_constructors parent
    >> check_child_constructors parent child
    >>= fun _ -> run_transfert_on_children class_table child

  (* Запуск transfert на ребенке и детях ребенка *)
  and run_transfert_on_children ht ch =
    let childs_of_child = ch.children_keys in
    monadic_list_iter childs_of_child
      (fun ch_ch_key ->
        transfert ch (Option.get (get_elem_if_present ht ch_ch_key)))
      ()

  let do_inheritance ht =
    let obj_r = Option.get (get_elem_if_present ht "Object") in
    let processing ch_key =
      transfert obj_r (Option.get (get_elem_if_present ht ch_key))
    in
    monadic_list_iter obj_r.children_keys processing ht

  let load cd_list =
    match cd_list with
    | [] -> error "Syntax error or empty file"
    | _ ->
        prepare_object class_table >>= fun table_with_object ->
        c_table_add cd_list table_with_object
        >>= fun table_with_added_classes ->
        update_child_keys table_with_added_classes >>= fun updated_table ->
        do_inheritance updated_table
end

module Main (M : MONADERROR) = struct
  open M

  type variable = {
    v_type : type_t;
    v_key : key_t;
    is_mutable : bool;
    v_value : value;
    scope_level : int;
  }
  [@@deriving show { with_path = false }]

  type context = {
    cur_object : obj_ref;
    prev_object : obj_ref option;
    var_table : (key_t, variable) Hashtbl_p.t;
    last_expr_result : value option;
    incremented : key_t list;
    decremented : key_t list;
    was_break : bool;
    was_continue : bool;
    was_return : bool;
    curr_method_type : type_t;
    (* Посмотрим, мб не надо *)
    is_main : bool;
    (* Считаем вложенные циклы *)
    cycle_cnt : int;
    scope_level : int;
  }
  [@@deriving show { with_path = false }]

  let make_context cur_object var_table =
    return
      {
        cur_object;
        prev_object = None;
        var_table;
        last_expr_result = None;
        incremented = [];
        decremented = [];
        was_break = false;
        was_continue = false;
        was_return = false;
        curr_method_type = Void;
        is_main = true;
        cycle_cnt = 0;
        scope_level = 0;
      }

  let find_class_with_main ht =
    return
      (List.find
         (fun c -> Hashtbl.mem c.method_table "main@@")
         (convert_table_to_list ht))

  let rec expr_type_check : expr -> context -> type_t M.t =
   fun t_expr ctx ->
    match t_expr with
    | Add (left, right) -> (
        expr_type_check left ctx >>= fun lt ->
        match lt with
        | Int -> (
            expr_type_check right ctx >>= fun rt ->
            match rt with
            | Int -> return Int
            | String -> return String
            | _ -> error "Wrong type: must be Int or String" )
        | String -> (
            expr_type_check right ctx >>= fun rt ->
            match rt with
            | Int | String -> return String
            | _ -> error "Wrong type: must be Int or String" )
        | _ -> error "Wrong type: must be Int or String" )
    | Sub (left, right)
    | Div (left, right)
    | Mod (left, right)
    | Mult (left, right) -> (
        expr_type_check left ctx >>= fun lt ->
        match lt with
        | Int -> (
            expr_type_check right ctx >>= fun rt ->
            match rt with
            | Int -> return Int
            | _ -> error "Wrong type: must be Int" )
        | _ -> error "Wrong type: must be Int" )
    | PostDec value | PostInc value | PrefDec value | PrefInc value -> (
        expr_type_check value ctx >>= fun vt ->
        match vt with Int -> return Int | _ -> error "Wrong type: must be Int" )
    | And (left, right) | Or (left, right) -> (
        expr_type_check left ctx >>= fun lt ->
        match lt with
        | Bool -> (
            expr_type_check right ctx >>= fun rt ->
            match rt with
            | Bool -> return Bool
            | _ -> error "Wrong type: must be Bool" )
        | _ -> error "Wrong type: must be Bool" )
    | Not value -> (
        expr_type_check value ctx >>= fun vt ->
        match vt with
        | Bool -> return Bool
        | _ -> error "Wrong type: must be Bool" )
    | Less (left, right)
    | More (left, right)
    | LessOrEqual (left, right)
    | MoreOrEqual (left, right) -> (
        expr_type_check left ctx >>= fun lt ->
        match lt with
        | Int -> (
            expr_type_check right ctx >>= fun rt ->
            match rt with
            | Int -> return Bool
            | _ -> error "Wrong type: must be Int" )
        | _ -> error "Wrong type: must be Int" )
    | Equal (left, right) | NotEqual (left, right) -> (
        expr_type_check left ctx >>= fun lt ->
        match lt with
        | Int -> (
            expr_type_check right ctx >>= fun rt ->
            match rt with
            | Int -> return Bool
            | _ -> error "Wrong type: must be Int" )
        | String -> (
            expr_type_check right ctx >>= fun rt ->
            match rt with
            | String -> return Bool
            | _ -> error "Wrong type: must be String" )
        | Bool -> (
            expr_type_check right ctx >>= fun rt ->
            match rt with
            | Bool -> return Bool
            | _ -> error "Wrong type: must be Bool" )
        | Array arr_lt -> (
            expr_type_check right ctx >>= fun rt ->
            match rt with
            | Array arr_rt when arr_lt = arr_rt -> return Bool
            | _ -> error "Wrong type: must be Array with right type!" )
        | ClassName ls -> (
            expr_type_check right ctx >>= fun rt ->
            match rt with
            | ClassName rs when ls = rs -> return Bool
            | ClassName "null" -> return Bool
            | _ -> error "Wrong class type!" )
        | _ -> error "Wrong type in equal-expression!" )
    | Null -> return (ClassName "null")
    | This -> (
        match ctx.cur_object with
        | RObj { class_key = k; _ } -> return (ClassName k)
        | RNull -> error "Null object in context!" )
    | Super -> (
        match ctx.cur_object with
        | RObj { class_key = k; _ } ->
            let par_k =
              Option.get
                (Option.get (get_elem_if_present class_table k)).parent_key
            in
            return (ClassName par_k)
        | RNull -> error "Null object in context!" )
    | CallMethod (Super, _) -> return Void
    | CallMethod (This, _) -> return Void
    (* По нашей модели такой вызов мог произойти только внутри какого-то объекта *)
    | CallMethod (Identifier m_ident, args) -> (
        let curr_obj_key =
          match ctx.cur_object with
          | RNull -> "null"
          | RObj { class_key = key; field_ref_table = _ } -> key
        in

        let curr_class =
          Option.get (get_elem_if_present class_table curr_obj_key)
        in
        make_type_string args ctx >>= fun args_string ->
        let m_key = m_ident ^ args_string ^ "@@" in
        match get_elem_if_present curr_class.method_table m_key with
        | None -> error "No such method in class!"
        | Some mr -> return mr.m_type )
    | FieldAccess (obj_expr, Identifier f_key) -> (
        expr_type_check obj_expr ctx >>= fun obj_c ->
        match obj_c with
        | ClassName "null" -> error "NullPointerException"
        | ClassName obj_key -> (
            let obj_class =
              Option.get (get_elem_if_present class_table obj_key)
            in
            let var_field_o = get_elem_if_present obj_class.field_table f_key in
            match var_field_o with
            | None -> error "No such field in class!"
            | Some var_field -> return var_field.f_type )
        | _ -> error "Wrong type: must be object reference" )
    | FieldAccess (obj_expr, CallMethod (Identifier m_ident, args)) -> (
        (* Чекаем то, что перед точкой - это должен быть объект *)
        expr_type_check obj_expr ctx
        >>= fun obj_c ->
        match obj_c with
        | ClassName "null" -> error "NullPointerException"
        | ClassName obj_key -> (
            make_type_string args ctx >>= fun args_string ->
            let m_key = m_ident ^ args_string ^ "@@" in
            let obj_class =
              Option.get (get_elem_if_present class_table obj_key)
            in
            let method_o = get_elem_if_present obj_class.method_table m_key in
            match method_o with
            | None -> error "No such method with this name and types!"
            | Some meth_r -> return meth_r.m_type )
        | _ -> error "Wrong type: must be object reference" )
    | ArrayAccess (arr_expr, index_expr) -> (
        expr_type_check index_expr ctx >>= fun ind_t ->
        match ind_t with
        | Int -> (
            expr_type_check arr_expr ctx >>= fun arr_t ->
            match arr_t with
            | Array t -> return t
            | _ -> error "Wrong type, must be Array!" )
        | _ -> error "Wrong type: must be Int" )
    | ArrayCreateSized (arr_type, size) -> (
        expr_type_check size ctx >>= fun size_t ->
        match size_t with
        | Int -> (
            match arr_type with
            | Void -> error "Wrong type of array"
            | Array _ -> error "Wrong type of Array"
            | _ -> return arr_type )
        | _ -> error "Wrong type: size must be Int" )
    | ArrayCreateElements (arr_type, elems) ->
        let rec process_list_el = function
          | [] -> return arr_type
          | el :: els ->
              expr_type_check el ctx >>= fun el_type ->
              if arr_type = el_type then process_list_el els
              else error "Wrong type of array element"
        in
        process_list_el elems
    | ClassCreate (Name class_name, args) -> (
        match get_elem_if_present class_table class_name with
        | None -> error "No such class!"
        | Some cl_elem -> (
            (* Если аргументов у конструктора нет - все норм, пустой конструктор всегда имеется *)
            match args with
            | [] -> return (ClassName class_name)
            | _ -> (
                make_type_string args ctx >>= fun args_string ->
                let constr_key = class_name ^ args_string ^ "$$" in
                let consrt_o =
                  get_elem_if_present cl_elem.constructor_table constr_key
                in
                match consrt_o with
                | None -> error "No such constructor with this types!"
                | _ -> return (ClassName class_name) ) ) )
    | Identifier key -> (
        (* Смотрим среди локальных переменных контекста *)
        let var_o = get_elem_if_present ctx.var_table key in
        match var_o with
        (* Нет, а вдруг есть среди полей объекта текущего класса? *)
        | None -> (
            match ctx.cur_object with
            | RObj { class_key = _; field_ref_table = ft } -> (
                (* Смотрим в таблице полей конкретного объекта *)
                match get_elem_if_present ft key with
                | None -> error "No such variable or field with this name!"
                | Some fr -> return fr.f_type )
            | _ -> error "Current object must not be null!" )
        (* Нашли в таблице локальных переменных - возвращаем его тип *)
        | Some v -> return v.v_type )
    | Const value -> (
        match value with
        | VBool _ -> return Bool
        | VInt _ -> return Int
        | VString _ -> return String
        | VObjectRef RNull -> return (ClassName "null")
        | VObjectRef (RObj { class_key = ck; field_ref_table = _ }) ->
            return (ClassName ck)
        | _ -> error "Wrong const value" )
    | Assign (left, right) -> (
        expr_type_check left ctx >>= fun lt ->
        match lt with
        | Void -> error "Wrong assign type"
        | ClassName cleft_key -> (
            expr_type_check right ctx >>= fun rt ->
            match rt with
            (* Null можно присвоить объекту любого класса *)
            | ClassName "null" -> return (ClassName cleft_key)
            | ClassName cright_key ->
                (* Среди родителей правого надо найти левый *)
                check_classname_assign cleft_key cright_key
            | _ -> error "Wrong assign types!" )
        | Array (ClassName cleft_key) -> (
            expr_type_check right ctx >>= fun rt ->
            match rt with
            | Array (ClassName cright_key) ->
                (* Массивы ковариантны, значит с ними надо проверять деревья родителей также, как и в предыдущем случае *)
                check_classname_assign cleft_key cright_key
            | _ -> error "Wrong assign types" )
        | _ ->
            expr_type_check right ctx >>= fun rt ->
            if lt = rt then return rt else error "Wrong assign types!" )
    | _ -> error "Wrong_expression"

  and check_classname_assign cleft_key cright_key =
    (* Среди родителей правого надо найти левый. key - текущий ключ правого *)
    let rec check_parent_tree key =
      let clr_by_key = Option.get (get_elem_if_present class_table key) in
      (* Смотрим, чтобы было совпадение: смотрим текущий ключ с левым *)
      if clr_by_key.this_key = cleft_key then return (ClassName cright_key)
      else
        match clr_by_key.parent_key with
        | None -> error "Wrong assign type!"
        | Some par_k -> check_parent_tree par_k
    in
    (* Иерархию перебираем у правого *)
    check_parent_tree cright_key

  and make_type_string : expr list -> context -> key_t M.t =
   fun elist ctx ->
    let rec helper_type lst acc =
      match lst with
      | [] -> return acc
      | e :: es ->
          expr_type_check e ctx >>= fun cur_t ->
          helper_type es (acc ^ show_type_t cur_t)
    in
    helper_type elist ""

  (* |> fun res_str -> get res_str  *)

  let get_int_value = function VInt x -> x | _ -> 0

  let get_string_value = function VString s -> s | _ -> ""

  let get_bool_value = function VBool b -> b | _ -> false

  let get_obj_value = function VObjectRef o -> o | _ -> RNull

  let get_arr_value = function VArray l -> l | _ -> []

  let make_list_of_elem el size =
    let rec helper acc curr =
      match curr with 0 -> acc | x -> helper (el :: acc) (x - 1)
    in
    helper [] size

  let inc_scope_level ctx = { ctx with scope_level = ctx.scope_level + 1 }

  let dec_scope_level ctx = { ctx with scope_level = ctx.scope_level - 1 }

  let rec eval_stmt : stmt -> context -> context M.t =
   fun stmt sctx ->
    match stmt with
    | StmtBlock st_list ->
        let rec helper_eval : stmt list -> context -> context M.t =
         fun stl hctx ->
          match stl with
          | [] -> return hctx
          | st :: sts -> (
              match st with
              | (Break | Continue | Return _) when sts <> [] ->
                  error "There are unreachable statements"
              | _ when hctx.cycle_cnt >= 1 && hctx.was_break -> return hctx
              | _ when hctx.cycle_cnt >= 1 && hctx.was_continue -> return hctx
              | _ when hctx.was_return ->
                  return hctx
                  (*Счетчик return обновляется после выхода из метода*)
              | _ ->
                  eval_stmt st hctx >>= fun head_ctx -> helper_eval sts head_ctx
              )
        in
        let delete_scope_var : context -> context M.t =
         fun ctx ->
          let delete : key_t -> variable -> unit =
           fun key el ->
            if el.scope_level <> ctx.scope_level then
              Hashtbl.remove ctx.var_table key
          in
          Hashtbl.iter delete ctx.var_table;
          return ctx
        in
        helper_eval st_list sctx >>= fun sbctx -> delete_scope_var sbctx
    | While (bexpr, lstmt) -> (
        let rec loop s ctx =
          (* Сразу проверяем брейк, случился ли он, случился - выходим из цикла*)
          if ctx.was_break then
            match s with
            (* Если был блок - то еще надо уровень видимости понизить *)
            | StmtBlock _ ->
                return
                  (dec_scope_level
                     {
                       ctx with
                       was_break = false;
                       cycle_cnt = ctx.cycle_cnt - 1;
                     })
            | _ ->
                return
                  { ctx with was_break = false; cycle_cnt = ctx.cycle_cnt - 1 }
          else
            eval_expr bexpr ctx >>= fun bectx ->
            match bectx.last_expr_result with
            | Some (VBool false) -> (
                match s with
                | StmtBlock _ ->
                    return
                      (dec_scope_level
                         { bectx with cycle_cnt = ctx.cycle_cnt - 1 })
                | _ -> return { bectx with cycle_cnt = ctx.cycle_cnt - 1 } )
            | Some (VBool true) ->
                eval_stmt s bectx >>= fun lctx ->
                (* Вылетел return - все прерываем, возвращаем контекст *)
                if lctx.was_return then return lctx
                  (*Может вылететь continue - значит циклимся заново*)
                else if lctx.was_continue then
                  loop s { lctx with was_continue = false }
                else loop s lctx
            | _ -> error "Wrong expression type for while stametent"
        in
        match lstmt with
        | StmtBlock _ ->
            loop lstmt
              (inc_scope_level { sctx with cycle_cnt = sctx.cycle_cnt + 1 })
        | _ -> loop lstmt { sctx with cycle_cnt = sctx.cycle_cnt + 1 } )
    | Break ->
        (* Break не может быть в цикле - проверяем это, если все ок - то просто возвращаем контекст с установленным флагом *)
        if sctx.cycle_cnt <= 0 then error "No loop for break"
        else return { sctx with was_break = true }
    | Continue ->
        (* Continue не может быть в цикле - проверяем это, если все ок - то просто возвращаем контекст с установленным флагом *)
        if sctx.cycle_cnt <= 0 then error "No loop for continue"
        else return { sctx with was_continue = true }
    | If (bexpr, then_stmt, else_stmt_o) -> (
        eval_expr bexpr sctx >>= fun bectx ->
        match bectx.last_expr_result with
        | Some (VBool true) -> (
            match then_stmt with
            | StmtBlock _ ->
                eval_stmt then_stmt (inc_scope_level bectx) >>= fun tctx ->
                return (dec_scope_level tctx)
            | _ -> eval_stmt then_stmt bectx )
        | Some (VBool false) -> (
            match else_stmt_o with
            | Some else_stmt -> (
                match else_stmt with
                | StmtBlock _ ->
                    eval_stmt else_stmt (inc_scope_level bectx) >>= fun ectx ->
                    return (dec_scope_level ectx)
                | _ -> eval_stmt else_stmt bectx )
            | None -> return sctx )
        | _ -> error "Wrong type for condition statement" )
    | For (dec_stmt_o, bexpr_o, after_expr_list, body_stmt) ->
        (* С циклом for scope_level всегда увеличивается, не смотря на наличие/отсутствие блока тела *)
        ( match dec_stmt_o with
        | None -> return (inc_scope_level sctx)
        | Some dec_stmt -> eval_stmt dec_stmt (inc_scope_level sctx) )
        >>= fun dec_ctx ->
        let rec loop bs afs ctx =
          (* Сразу проверяем брейк, случился ли он, случился - выходим из цикла*)
          if ctx.was_break then
            return
              {
                ctx with
                was_break = false;
                cycle_cnt = ctx.cycle_cnt - 1;
                scope_level = ctx.scope_level - 1;
              }
          else
            (*Стандартно: смотрим результат бул-выражения, если true - вычислить тело и инкременты после*)
            ( match bexpr_o with
            | None -> return { ctx with last_expr_result = Some (VBool true) }
            | Some bexpr -> eval_expr bexpr ctx )
            >>= fun bectx ->
            match bectx.last_expr_result with
            (* Увидели false - значит уже не циклимся, возвращаем контекст с уменьшенным счетчиком вложенных циклов и scope*)
            | Some (VBool false) ->
                return
                  {
                    bectx with
                    cycle_cnt = bectx.cycle_cnt - 1;
                    scope_level = bectx.scope_level - 1;
                  }
            | Some (VBool true) ->
                let rec eval_inc_expr_list e_list c =
                  match e_list with
                  | [] -> return c
                  | e :: es -> (
                      (* Только таких типов могут быть выражения в части, выполняющейся после тела *)
                      match e with
                      | PostDec _ | PostInc _ | PrefDec _ | PrefInc _
                      | Assign (_, _)
                      | CallMethod (_, _)
                      | FieldAccess (_, CallMethod (_, _)) ->
                          eval_expr e c >>= fun ehctx ->
                          eval_inc_expr_list es ehctx
                      | _ -> error "Wrong expression for after body list" )
                in
                eval_stmt bs bectx >>= fun bdctx ->
                (* Вылетел return - все прерываем, возвращаем контекст *)
                if bdctx.was_return then return bdctx
                  (*Может вылететь continue - значит циклимся заново, инкременты не вычисляем*)
                else if bdctx.was_continue then
                  loop bs afs { bdctx with was_continue = false }
                else
                  (* Иначе можем просто вычислить инкременты после и сделать цикл *)
                  eval_inc_expr_list afs bdctx >>= fun after_ctx ->
                  loop bs afs after_ctx
            | _ -> error "Wrong condition type in for statement"
        in
        loop body_stmt after_expr_list dec_ctx
    | Return rexpr_o -> (
        match rexpr_o with
        (* Если нет никакого выражения - метод, в котором мы исполняемся, должен иметь тип Void *)
        | None when sctx.curr_method_type = Void ->
            (* Если тип Void - выходим со значением VVoid поставленным флагом, что был return *)
            return
              { sctx with last_expr_result = Some VVoid; was_return = true }
        | None -> error "Return value type mismatch"
        (* Если есть выражение - смотрим, чтобы тип совпадал с типом, возвращаемым методом *)
        | Some rexpr ->
            expr_type_check rexpr sctx >>= fun rexpr_type ->
            if rexpr_type <> sctx.curr_method_type then
              error "Return value type mismatch"
            else
              (* Возвращаем контекст, в котором есть результат выражения, но не забываем поставить флаг, что был return *)
              eval_expr rexpr sctx >>= fun rectx ->
              return { rectx with was_return = true } )
    | Expression sexpr -> (
        match sexpr with
        | PostDec _ | PostInc _ | PrefDec _ | PrefInc _
        | CallMethod (_, _)
        | FieldAccess (_, CallMethod (_, _))
        | Assign (_, _) ->
            eval_expr sexpr sctx >>= fun ectx -> return ectx
        | _ -> error "Wrong expression for statement" )
    | VarDec (final_mod_o, vars_type, var_list) ->
        let is_final : modifier option -> bool = function
          | Some Final -> true
          | _ -> false
        in
        let rec helper_vardec v_list vctx =
          match v_list with
          | [] -> return vctx
          | (Name name, var_expr_o) :: vs -> (
              match vctx.cur_object with
              | RNull -> error "Must be non-null object!"
              | RObj { class_key = _; field_ref_table = frt } -> (
                  if
                    (* Смотрим, чтобы подобного имени не было ни среди локальных переменных, ни среди полей класса *)
                    Hashtbl.mem vctx.var_table name || Hashtbl.mem frt name
                  then error "Variable with this name is already defined"
                  else
                    match var_expr_o with
                    (* Если ничего нет - инициализируем базовым значением *)
                    | None ->
                        Hashtbl.add vctx.var_table name
                          {
                            v_type = vars_type;
                            v_key = name;
                            is_mutable = is_final final_mod_o;
                            v_value = get_init_value_of_type vars_type;
                            scope_level = vctx.scope_level;
                          };
                        return vctx
                        (* Если что-то есть - присваиваем значение, вычисленное справа *)
                    | Some var_expr -> (
                        expr_type_check var_expr vctx >>= fun var_expr_type ->
                        (* Добавить в таблицу переменных контекста то, что в выражении переменной справа *)
                        let add_var ve =
                          eval_expr ve vctx >>= fun vare_ctx ->
                          Hashtbl.add vare_ctx.var_table name
                            {
                              v_type = var_expr_type;
                              v_key = name;
                              is_mutable = is_final final_mod_o;
                              v_value = Option.get vare_ctx.last_expr_result;
                              scope_level = vare_ctx.scope_level;
                            };
                          return vare_ctx
                        in
                        match var_expr_type with
                        (* Null можно присвоить любому объекту *)
                        | ClassName "null" -> (
                            match vars_type with
                            | ClassName _ -> add_var var_expr
                            | _ -> error "Wrong assign type in declaration" )
                        (* Если тип справа - класс, то надо аккуратно проверить тип, соблюдая наследование *)
                        | ClassName cright -> (
                            match vars_type with
                            | ClassName cleft ->
                                check_classname_assign cleft cright
                                (* Тип проверится нормально - тогда просто добавим *)
                                >> add_var var_expr
                            | _ -> error "Wrong assign type in declaration" )
                        (* Если тип справа - массив объектов класса, то тоже надо проверять наследование, т.к. есть ковариантность *)
                        | Array (ClassName cright) -> (
                            match vars_type with
                            | Array (ClassName cleft) ->
                                check_classname_assign cleft cright
                                >> add_var var_expr
                            | _ -> error "Wrong assign type in declaration" )
                        | _ when var_expr_type = vars_type -> add_var var_expr
                        | _ ->
                            error "Wrong value type for variable declared!"
                            >>= fun head_ctx -> helper_vardec vs head_ctx ) ) )
        in
        helper_vardec var_list sctx

  (*Not implemented *)
  and eval_expr : expr -> context -> context M.t =
   fun expr ctx ->
    let rec eval_e (* TODO: СДЕЛАТЬ ФУНКЦИЕЙ *) =
      let eval_op left right op =
        eval_expr left ctx >>= fun lctx ->
        eval_expr right lctx >>= fun rctx ->
        let l_value = Option.get lctx.last_expr_result in
        let r_value = Option.get rctx.last_expr_result in
        let new_value = op l_value r_value in
        try return { rctx with last_expr_result = Some new_value } with
        | Invalid_argument m -> error m
        | Division_by_zero -> error "Division by zero!"
      in
      let eval_un v_expr op =
        eval_expr v_expr ctx >>= fun vctx ->
        let v = Option.get vctx.last_expr_result in
        let new_v = op v in
        try return { vctx with last_expr_result = Some new_v }
        with Invalid_argument m -> error m
      in
      match expr with
      | Add (left, right) -> eval_op left right ( ++ )
      | Sub (left, right) -> eval_op left right ( -- )
      | Div (left, right) -> eval_op left right ( // )
      | Mod (left, right) -> eval_op left right ( %% )
      | And (left, right) -> eval_op left right ( &&& )
      | Or (left, right) -> eval_op left right ( ||| )
      | Not bexp -> eval_un bexp not_v
      | Less (left, right) -> eval_op left right ( <<< )
      | More (left, right) -> eval_op left right ( >>> )
      | LessOrEqual (left, right) -> eval_op left right ( <<== )
      | MoreOrEqual (left, right) -> eval_op left right ( >>== )
      | Equal (left, right) -> eval_op left right ( === )
      | NotEqual (left, right) -> eval_op left right ( !=! )
      | Const v -> return { ctx with last_expr_result = Some v }
      | Identifier id ->
          let var_by_id =
            Option.get (Hashtbl_p.get_elem_if_present ctx.var_table id)
          in
          return { ctx with last_expr_result = Some var_by_id.v_value }
      | Null -> return { ctx with last_expr_result = Some (VObjectRef RNull) }
      (*Должен быть после CallMethod (This, ...)!!!!*)
      | This ->
          return
            { ctx with last_expr_result = Some (VObjectRef ctx.cur_object) }
      | FieldAccess (obj_expr, Identifier f_key) -> (
          eval_expr obj_expr ctx >>= fun octx ->
          let obj = Option.get octx.last_expr_result in
          match obj with
          | VObjectRef (RObj { class_key = _; field_ref_table = frt }) ->
              (* Смело пользуемся Option.get, потому что перед этим была проверка типов, в ней проверяется наличие этого поля у класса*)
              let fld = Option.get (Hashtbl.find_opt frt f_key) in
              return { octx with last_expr_result = Some fld.f_value }
          | _ -> error "Must be non-null object!" )
      | FieldAccess (obj_expr, CallMethod (Identifier m_name, args)) -> (
          eval_expr obj_expr ctx >>= fun octx ->
          let obj = Option.get octx.last_expr_result in
          match obj with
          | VObjectRef obj_ref -> (
              match obj_ref with
              | RNull -> error "NullPointerException"
              | RObj { class_key = cl_k; field_ref_table = _ } -> (
                  make_type_string args octx >>= fun args_string ->
                  let method_key = m_name ^ args_string ^ "@@" in
                  (* Смотрим класс, к которому принадлежит объект, с проверкой на существование *)
                  match get_elem_if_present class_table cl_k with
                  | None -> error "No such object in class!"
                  | Some obj_class -> (
                      (* Смотрим метод у этого класса, опять с проверкой на существование *)
                      match
                        get_elem_if_present obj_class.method_table method_key
                      with
                      | None -> error "No such method in class!"
                      | Some mr ->
                          (* Смотреть тело на None не нужно, это только если метод абстрактный,
                             а проверка на то, чтобы нельзя было создать абстрактный класс - в вычислении ClassCreate *)
                          let m_body = Option.get mr.body in
                          let m_arg_list = mr.args in
                          let new_var_table : (key_t, variable) Hashtbl_p.t =
                            Hashtbl.create 100
                          in
                          let prepare_table_with_args :
                              (key_t, variable) Hashtbl_p.t ->
                              expr list ->
                              ((key_t, variable) Hashtbl_p.t * context) M.t =
                           fun ht args_l ->
                            let rec helper_add h_ht arg_expr_list arg_mr_list
                                help_ctx =
                              match (arg_expr_list, arg_mr_list) with
                              (* Одновременно бежим по двум спискам: списку выражений, переданных в метод и списку параметров в записи метода у класса.
                                 Гарантируется из предыдущих проверок, что длина списков будет одинакова *)
                              | [], [] -> return (h_ht, help_ctx)
                              | ( arg :: args,
                                  (head_type, Name head_name) :: pairs ) ->
                                  eval_expr arg help_ctx >>= fun he_ctx ->
                                  Hashtbl.add h_ht head_name
                                    {
                                      v_type = head_type;
                                      v_key = head_name;
                                      is_mutable = true;
                                      v_value =
                                        Option.get he_ctx.last_expr_result;
                                      scope_level = 0;
                                    };
                                  helper_add h_ht args pairs he_ctx
                              | _ -> error "No such method in class!"
                            in
                            helper_add ht args_l m_arg_list octx
                          in
                          prepare_table_with_args new_var_table args
                          >>= fun (new_vt, new_ctx) ->
                          (* Тело метода исполняется в новом контексте с переменными из переданных аргументов *)
                          eval_stmt m_body
                            {
                              cur_object = obj_ref;
                              prev_object = Some octx.cur_object;
                              var_table = new_vt;
                              last_expr_result = None;
                              incremented = [];
                              decremented = [];
                              was_break = false;
                              was_continue = false;
                              was_return = false;
                              curr_method_type = mr.m_type;
                              is_main = false;
                              cycle_cnt = 0;
                              scope_level = 0;
                            }
                          (* От контекста нас интересует только результат работы метода *)
                          >>=
                          fun m_res_ctx ->
                          (* После отработки метода возвращаем контекст с результатом работы метода
                             Если внутри метода менялись какие-то состояния объектов -
                             они должны поменяться исходя из мутабельности хеш-таблиц в результате присваиваний *)
                          return
                            {
                              new_ctx with
                              last_expr_result = m_res_ctx.last_expr_result;
                            } ) ) )
          | _ -> error "Must be non-null object!" )
      (* Это в случае, если вызываем внутри какого-то объекта. This нам вернет этот объект при вычислении *)
      | CallMethod (Identifier m, args) ->
          eval_expr (FieldAccess (This, CallMethod (Identifier m, args))) ctx
      | ArrayAccess (arr_expr, index_expr) -> (
          eval_expr arr_expr ctx >>= fun arrctx ->
          eval_expr index_expr ctx >>= fun indctx ->
          let arr_v = Option.get arrctx.last_expr_result in
          let ind_v = Option.get indctx.last_expr_result in
          match arr_v with
          | VArray values -> (
              match ind_v with
              | VInt i when i < 0 || i > List.length values ->
                  error "ArrayOutOfBoundsException"
              | VInt i ->
                  return
                    { indctx with last_expr_result = Some (List.nth values i) }
              | _ -> error "Index must be int!" )
          | _ -> error "Must be array!" )
      | ArrayCreateSized (arr_type, size_expr) -> (
          eval_expr size_expr ctx >>= fun szctx ->
          let size_v = Option.get szctx.last_expr_result in
          let init_v = get_init_value_of_type arr_type in
          match size_v with
          | VInt size ->
              return
                {
                  szctx with
                  last_expr_result =
                    Some (VArray (make_list_of_elem init_v size));
                }
          | _ -> error "Size must be int!" )
      | ArrayCreateElements (_, expr_list) ->
          let make_val_list ex_list fctx =
            let rec helper acc e_lst hctx =
              match e_lst with
              | [] -> return acc
              | e :: es ->
                  eval_expr e hctx >>= fun ectx ->
                  let head_val = Option.get ectx.last_expr_result in
                  helper (head_val :: acc) es ectx
            in
            helper [] ex_list fctx
          in
          let new_v_list = get (make_val_list expr_list ctx) in
          return { ctx with last_expr_result = Some (VArray new_v_list) }
      | _ -> return ctx
    in
    return ctx

  let execute : (key_t, class_r) Hashtbl.t -> context M.t =
   fun ht ->
    find_class_with_main ht >>= fun cl ->
    make_context
      (RObj { class_key = cl.this_key; field_ref_table = Hashtbl.create 10 })
      (Hashtbl.create 10)
    >>= fun ctx ->
    let main = Hashtbl.find cl.method_table "main@@" in
    let body_main = Option.get main.body in
    eval_stmt body_main ctx
end
