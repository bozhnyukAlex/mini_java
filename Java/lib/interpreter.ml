open Ast
open Parser
open Hashtbl

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( >> ) : 'a t -> 'b t -> 'b t

  val get : 'a t -> 'a
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
                    let f_value = None in
                    (* В качестве ключа выступает имя поля *)
                    add_with_check field_table key
                      { f_type; key; is_mutable; f_value; sub_tree }
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
  }

  type context = {
    cur_object : obj_ref;
    var_table : (key_t, variable) Hashtbl.t;
  }

  let make_context cur_object var_table = return { cur_object; var_table }

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
            | ClassName "this" -> return Bool
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
    | FieldAccess (obj_expr, Identifier f_key) -> (
        expr_type_check obj_expr ctx >>= fun obj_c ->
        match obj_c with
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
        | ClassName obj_key -> (
            let m_key = m_ident ^ make_type_string args ctx ^ "@@" in
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
        | Int -> expr_type_check arr_expr ctx >>= fun arr_t -> return arr_t
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
                let args_string = make_type_string args ctx in
                let constr_key = class_name ^ args_string ^ "$$" in
                let consrt_o =
                  get_elem_if_present cl_elem.constructor_table constr_key
                in
                match consrt_o with
                | None -> error "No such constructor with this types!"
                | _ -> return (ClassName class_name) ) ) )
    | Identifier key -> (
        let var_o = get_elem_if_present ctx.var_table key in
        match var_o with
        | None -> error "No such variable!"
        | Some v -> return v.v_type )
    | Const value -> (
        match value with
        | VBool _ -> return Bool
        | VInt _ -> return Int
        | VString _ -> return String
        | VObjectRef RNull -> return (ClassName "null")
        | VObjectRef (RObj { class_key = ck; field_ref_list = _ }) ->
            return (ClassName ck)
        | _ -> error "Wrong value" )
    | Assign (left, right) -> (
        expr_type_check left ctx >>= fun lt ->
        match lt with
        | Void -> error "Wrong assign type"
        | ClassName cleft_key -> (
            expr_type_check right ctx >>= fun rt ->
            match rt with
            | ClassName cright_key ->
                let rec check_parent_tree key =
                  let clr_by_key =
                    Option.get (get_elem_if_present class_table key)
                  in
                  if clr_by_key.this_key = cright_key then
                    return (ClassName cright_key)
                  else
                    match clr_by_key.parent_key with
                    | None -> error "Wrong assign type!"
                    | Some par_k -> check_parent_tree par_k
                in
                check_parent_tree cleft_key
            | _ -> error "Wrong assign types!" )
        | _ ->
            expr_type_check right ctx >>= fun rt ->
            if lt = rt then return rt else error "Wrong assign types!" )
    | _ -> error "Wrong_expression"

  and make_type_string : expr list -> context -> key_t =
   fun elist ctx ->
    let rec helper_type lst acc =
      match lst with
      | [] -> return acc
      | e :: es ->
          expr_type_check e ctx >>= fun cur_t ->
          helper_type es (acc ^ show_type_t cur_t)
    in
    helper_type elist "" |> fun res_str -> get res_str

  (* Not implemented *)
  let interp_stmt : stmt -> context -> context M.t = fun s c -> return c

  (*Not implemented *)
  and interp_expr (* Должна быть инфа о inc и dec *) :
      expr -> context -> context M.t =
   fun e c -> return c

  let execute : (key_t, class_r) Hashtbl.t -> context M.t =
   fun ht ->
    find_class_with_main ht >>= fun cl ->
    make_context
      (RObj { class_key = cl.this_key; field_ref_list = [] })
      (Hashtbl.create 10)
    >>= fun ctx ->
    let main = Hashtbl.find cl.method_table "main@@" in
    let body_main = Option.get main.body in
    interp_stmt body_main ctx
end
