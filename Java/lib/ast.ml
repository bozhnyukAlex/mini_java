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
and field_ref = { key : string; f_type : type_t; f_value : value }

(* Ссылка на объект - может быть null или запись именем класса (он же ключ) и ссылкой на поля *)
and obj_ref =
  | RNull
  | RObj of { class_key : string; field_ref_list : field_ref list }

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
  | VarDec of type_t * (name * expr option) list
  | Expression of expr
  | Throw of expr
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
