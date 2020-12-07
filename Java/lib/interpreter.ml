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

type key_t = string

type constructor_r = { args : (type_t * name) list; body : stmt }

type field_r = {
  f_type : type_t;
  key : key_t;
  is_abstract : bool;
  is_mutable : bool;
  mutable f_value : value option;
  sub_tree : expr option;
}

type method_r = {
  m_type : type_t;
  is_abstract : bool;
  is_overridable : bool;
  args : (type_t * name) list;
  key : key_t;
  body : stmt option;
}

type class_r = {
  mutable field_table : (key_t, field_r) Hashtbl.t;
  mutable method_table : (key_t, method_r) Hashtbl.t;
  mutable constructor_table : (key_t, constructor_r) Hashtbl.t;
  mutable children_keys : key_t list;
  is_abstract : bool;
  is_inheritable : bool;
  parent_key : key_t option;
}

let class_table : (key_t, class_r) Hashtbl.t = Hashtbl.create 1024

type record =
  | FieldR of field_r
  | MethodR of method_r
  | ConstructorR of constructor_r

let convert_name_to_key = function Some (Name x) -> Some x | None -> None

module Preparation (M : MONADERROR) = struct
  let is_abstract = List.mem Abstract

  let is_final = List.mem Final

  let is_override = List.mem Override

  let is_static = List.mem Static

  let is_public = List.mem Public

  let get_type_list = List.map (function t, _ -> t)

  let add_with_check ht key value e_message =
    let open M in
    match Hashtbl.find_all ht key with
    | [] -> return (Hashtbl.add ht key value)
    | _ -> error e_message

  let c_table_adding : class_dec list -> unit M.t =
   fun cd_list ->
    let open M in
    let add_to_class_table : class_dec -> unit M.t = function
      | Class (ml, Name cl_n, parent_o, fields) ->
          let is_new =
            match Hashtbl.find_all class_table cl_n with
            | [] -> true
            | _ -> false
          in
          let modif_correct = is_override ml = false && is_static ml = false in
          let m_table = Hashtbl.create 1024 in
          let f_table = Hashtbl.create 1024 in
          let c_table = Hashtbl.create 1024 in
          if is_new = false && modif_correct = false then
            error "Similar classes"
          else
            let add_field : modifier list * field -> unit M.t = function
              | f_ms, VarField (f_t, pairs) ->
                  let rec helper = function
                    | [] -> return ()
                    | (Name f_name, expr_o) :: ps ->
                        add_with_check f_table f_name
                          {
                            f_type = f_t;
                            key = f_name;
                            is_abstract = is_abstract f_ms;
                            is_mutable = is_final f_ms;
                            f_value = None;
                            sub_tree = expr_o;
                          }
                          "Similar fields"
                        >>= fun _ -> helper ps
                  in
                  helper pairs
              | m_ms, Method (m_t, Name name, args_list, m_body) -> (
                  let method_key =
                    String.concat ""
                      (name :: List.map show_type_t (get_type_list args_list))
                  in
                  if is_abstract m_ms = true then
                    match m_body with
                    | Some _ -> error "Abstract method cannot have body!"
                    | None ->
                        if is_abstract ml then
                          add_with_check m_table method_key
                            {
                              m_type = m_t;
                              is_abstract = true;
                              is_overridable = true;
                              args = args_list;
                              key = method_key;
                              body = m_body;
                            }
                            "Method with this type exists"
                        else error "Abstract method in non-abstract class"
                  else
                    match m_body with
                    | Some _ ->
                        add_with_check m_table method_key
                          {
                            m_type = m_t;
                            is_abstract = false;
                            is_overridable = is_override m_ms;
                            args = args_list;
                            key = method_key;
                            body = m_body;
                          }
                          "Method with this type exists"
                    | None -> error "No body of non-abstract method" )
              | c_ms, Constructor (Name name, args_list, c_body) ->
                  let constr_key =
                    String.concat ""
                      (name :: List.map show_type_t (get_type_list args_list))
                  in
                  let is_correct =
                    (not (is_abstract c_ms))
                    && (not (is_final c_ms))
                    && is_public c_ms
                    && (not (is_static c_ms))
                    && name = cl_n
                  in
                  if is_correct then
                    add_with_check c_table constr_key
                      { args = args_list; body = c_body }
                      "Constructor with this type exists"
                  else error "Constructor syntax error!"
            in
            let rec helper_add = function
              | [] -> return ()
              | f :: fs -> add_field f >>= fun _ -> helper_add fs
            in
            helper_add fields >>= fun _ ->
            return
              (Hashtbl.add class_table cl_n
                 {
                   field_table = f_table;
                   method_table = m_table;
                   constructor_table = c_table;
                   children_keys = [];
                   is_abstract = is_abstract ml;
                   is_inheritable = is_final ml;
                   parent_key = convert_name_to_key parent_o;
                 })
    in
    let rec helper_classes_add = function
      | [] -> return ()
      | c :: cs -> add_to_class_table c >>= fun _ -> helper_classes_add cs
    in
    helper_classes_add cd_list

  let update_child_keys_exn : unit M.t =
    let open M in
    let update key = function
      | {
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
              (*raises exception if not found, need to be handled*)
              let parent = Hashtbl.find class_table p_key in
              let par_ch_keys = parent.children_keys in
              parent.children_keys <- key :: par_ch_keys )
    in
    return (Hashtbl.iter update class_table)
end
