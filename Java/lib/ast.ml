type modifier = Public | Static | Final | Abstract | Override
[@@deriving show { with_path = false }]

type type_t =
  | Int
  | Void
  | ClassName of string
  | String
  | Array of type_t
  | Bool
[@@deriving show { with_path = false }]

type value =
  | VBool of bool
  | VInt of int
  | VArray of value list
  | VVoid
  | VString of string
  | VObjectRef of obj_ref
[@@deriving show { with_path = false }]

(* Поле внутри созданного объекта *)
and field_ref = {
  key : string;
  f_type : type_t;
  f_value : value;
  is_mutable : bool;
}

(* Ссылка на объект - может быть null или запись именем класса (он же ключ) и ссылкой на поля *)
and obj_ref =
  | RNull
  | RObj of {
      class_key : string;
      field_ref_table : (string, field_ref) Hashtbl_p.t;
    }

let ( ++ ) v1 v2 =
  match (v1, v2) with
  | VInt x, VInt y -> VInt (x + y)
  | VString x, VString y -> VString (x ^ y)
  | VInt x, VString y -> VString (string_of_int x ^ y)
  | VString x, VInt y -> VString (x ^ string_of_int y)
  | _, _ -> raise (Invalid_argument "Wrong argument types for adding!")

let ( -- ) v1 v2 =
  match (v1, v2) with
  | VInt x, VInt y -> VInt (x - y)
  | _, _ -> raise (Invalid_argument "Wrong argument types for subtraction!")

let ( ** ) v1 v2 =
  match (v1, v2) with
  | VInt x, VInt y -> VInt (x * y)
  | _, _ -> raise (Invalid_argument "Wrong argument types for multiplication!")

let ( // ) v1 v2 =
  match (v1, v2) with
  | VInt _, VInt y when y = 0 -> raise Division_by_zero
  | VInt x, VInt y -> VInt (x / y)
  | _, _ -> raise (Invalid_argument "Wrong argument types for subtraction!")

let ( %% ) v1 v2 =
  match (v1, v2) with
  | VInt _, VInt y when y = 0 -> raise Division_by_zero
  | VInt x, VInt y -> VInt (x mod y)
  | _, _ -> raise (Invalid_argument "Wrong argument types for mod operator!")

let ( >>> ) v1 v2 =
  match (v1, v2) with
  | VInt x, VInt y -> VBool (x > y)
  | _ -> raise (Invalid_argument "Wrong type for ordering!")

let ( <<< ) v1 v2 =
  match (v1, v2) with
  | VInt x, VInt y -> VBool (x < y)
  | _ -> raise (Invalid_argument "Wrong type for ordering!")

let ( <<== ) v1 v2 =
  match (v1, v2) with
  | VInt x, VInt y -> VBool (x <= y)
  | _ -> raise (Invalid_argument "Wrong type for ordering!")

let ( >>== ) v1 v2 =
  match (v1, v2) with
  | VInt x, VInt y -> VBool (x >= y)
  | _ -> raise (Invalid_argument "Wrong type for ordering!")

let ( &&& ) v1 v2 =
  match (v1, v2) with
  | VBool x, VBool y -> VBool (x && y)
  | _, _ -> raise (Invalid_argument "Wrong types for && operator!")

let ( ||| ) v1 v2 =
  match (v1, v2) with
  | VBool x, VBool y -> VBool (x || y)
  | _, _ -> raise (Invalid_argument "Wrong types for || operator!")

let not_v = function
  | VBool x -> VBool (not x)
  | _ -> raise (Invalid_argument "Wrong types for NOT operator!")

let rec equal_value v1 v2 =
  match (v1, v2) with
  | VInt x, VInt y -> x = y
  | VBool x, VBool y -> x = y
  | VVoid, VVoid -> true
  | VString s, VString t -> s = t
  | VObjectRef x, VObjectRef y -> x = y
  | VArray x_values, VArray y_values ->
      List.for_all2 equal_value x_values y_values
  | _ -> raise (Invalid_argument "Wrong types for equality!")

let rec ( === ) v1 v2 =
  match (v1, v2) with
  | VInt x, VInt y -> VBool (x = y)
  | VBool x, VBool y -> VBool (x = y)
  | VVoid, VVoid -> VBool true
  | VString s, VString t -> VBool (s = t)
  | VObjectRef x, VObjectRef y -> VBool (x = y)
  | VArray x_values, VArray y_values ->
      VBool (List.for_all2 equal_value x_values y_values)
  | _ -> raise (Invalid_argument "Wrong types for equality!")

let rec ( !=! ) v1 v2 = not_v (v1 === v2)

let get_init_value_of_type = function
  | Int -> VInt 0
  | String -> VString ""
  | ClassName _ -> VObjectRef RNull
  | Bool -> VBool false
  | Void -> VVoid
  | Array _ -> VArray []

type name = Name of string [@@deriving show { with_path = false }]

type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | PrefInc of expr
  | PrefDec of expr
  | PostInc of expr
  | PostDec of expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Equal of expr * expr
  | NotEqual of expr * expr
  | Less of expr * expr
  | More of expr * expr
  | LessOrEqual of expr * expr
  | MoreOrEqual of expr * expr
  | ClassCreate of name * expr list (*new clName(argList)*)
  | ArrayCreateSized of type_t * expr (*new arrType[cntExpr]*)
  | ArrayCreateElements of type_t * expr list (*new arrType[] {expr, ... , expr}*)
  | CallMethod of expr * expr list (*this(...), super(...) ident(...)*)
  | Identifier of string
  | Const of value
  | This
  | Super
  | Null
  | FieldAccess of expr * expr
  | ArrayAccess of expr * expr (*arr_name[index]*)
  | Assign of expr * expr
[@@deriving show { with_path = false }]

and stmt =
  | If of expr * stmt * stmt option (*cond * thenStat * elseStat*)
  | While of expr * stmt (* cond * body *)
  | For of stmt option * expr option * expr list * stmt (* varDec * expr * afterBody * body *)
  | Break
  | Continue
  | Return of expr option (* result *)
  | StmtBlock of stmt list
  | VarDec of modifier option * type_t * (name * expr option) list
  | Expression of expr
[@@deriving show { with_path = false }]

and field =
  | Method of
      type_t
      * name
      * (type_t * name) list
      (*List of pairs (type, identificator)*)
      * stmt option (*Statement block*)
  | VarField of type_t * (name * expr option) list
  | Constructor of name * (type_t * name) list * stmt
[@@deriving show { with_path = false }]

and class_dec =
  | Class of
      modifier list
      * name (*class name*)
      * name option
      (*Parent class_name*)
      * (modifier list * field) list
(* class body *) [@@deriving show { with_path = false }]

let get_field_list = function Class (_, _, _, f_list) -> List.map snd f_list

let convert_elem_pair_list = function
  | t, p_list -> List.map (fun p -> match p with s, f -> (t, s, f)) p_list

let get_var_field_pairs_list_typed cd =
  List.concat
    (List.map convert_elem_pair_list
       (List.filter_map
          (fun f ->
            match f with
            | VarField (t, pair_list) -> Some (t, pair_list)
            | _ -> None)
          (get_field_list cd)))
