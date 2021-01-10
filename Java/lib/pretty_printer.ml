open Format
open Ast

let pp_val ppf = function
  | VBool b -> fprintf ppf "%b" b
  | VChar c -> fprintf ppf "%c" c
  | VInt i -> fprintf ppf "%d" i
  | VString s -> fprintf ppf "\"%s\"" s
  | _ -> ()

let rec pp_type ppf = function
  | Int -> fprintf ppf "int"
  | Char -> fprintf ppf "char"
  | Bool -> fprintf ppf "boolean"
  | Void -> fprintf ppf "void"
  | ClassName s -> fprintf ppf "%s" s
  | Array t -> fprintf ppf "%a[]" pp_type t

let pp_modif ppf = function
  | Public -> fprintf ppf "%s" "public"
  | Static -> fprintf ppf "%s" "static"
  | Final -> fprintf ppf "%s" "final"
  | Abstract -> fprintf ppf "%s" "abstract"
  | Override -> fprintf ppf "%s" "@Override"

let is_add_or_sub = function Add (_, _) | Sub (_, _) -> true | _ -> false

let is_or = function Or (_, _) -> true | _ -> false

let is_const_or_id = function Const _ | Identifier _ -> true | _ -> false

let is_create = function ClassCreate (_, _) -> true | _ -> false

let pp_sep_comma ppf () = fprintf ppf ", "

let pp_name ppf = function Name n -> fprintf ppf "%s" n

let rec pp_exp ppf = function
  | Add (left, right) -> fprintf ppf "%a + %a" pp_exp left pp_exp right
  | Sub (left, right) -> fprintf ppf "%a - %a" pp_exp left pp_exp right
  | Mult (left, right) ->
      if is_add_or_sub left then (
        fprintf ppf "%a * " pp_exp_par left;
        if is_add_or_sub right then fprintf ppf "%a" pp_exp_par right
        else fprintf ppf "%a" pp_exp right )
      else (
        fprintf ppf "%a * " pp_exp left;
        if is_add_or_sub right then fprintf ppf "%a" pp_exp_par right
        else fprintf ppf "%a" pp_exp right )
  | Div (left, right) ->
      if is_add_or_sub left then (
        fprintf ppf "%a / " pp_exp_par left;
        if is_add_or_sub right then fprintf ppf "%a" pp_exp_par right
        else fprintf ppf "%a" pp_exp right )
      else (
        fprintf ppf "%a / " pp_exp left;
        if is_add_or_sub right then fprintf ppf "%a" pp_exp_par right
        else fprintf ppf "%a" pp_exp right )
  | Mod (left, right) ->
      if is_add_or_sub left then (
        fprintf ppf "%a %% " pp_exp_par left;
        if is_add_or_sub right then fprintf ppf "%a" pp_exp_par right
        else fprintf ppf "%a" pp_exp right )
      else (
        fprintf ppf "%a %% " pp_exp left;
        if is_add_or_sub right then fprintf ppf "%a" pp_exp_par right
        else fprintf ppf "%a" pp_exp right )
  | And (left, right) ->
      if is_or left then (
        fprintf ppf "%a && " pp_exp_par left;
        if is_or right then fprintf ppf "%a" pp_exp_par right
        else fprintf ppf "%a" pp_exp right )
      else (
        fprintf ppf "%a && " pp_exp left;
        if is_or right then fprintf ppf "%a" pp_exp_par right
        else fprintf ppf "%a" pp_exp right )
  | Or (left, right) -> fprintf ppf "%a || %a" pp_exp left pp_exp right
  | Not be ->
      if is_const_or_id be then fprintf ppf "!%a" pp_exp be
      else fprintf ppf "!%a" pp_exp_par be
  | Less (left, right) -> fprintf ppf "%a < %a" pp_exp left pp_exp right
  | More (left, right) -> fprintf ppf "%a > %a" pp_exp left pp_exp right
  | LessOrEqual (left, right) -> fprintf ppf "%a <= %a" pp_exp left pp_exp right
  | MoreOrEqual (left, right) -> fprintf ppf "%a >= %a" pp_exp left pp_exp right
  | Equal (left, right) -> fprintf ppf "%a == %a" pp_exp left pp_exp right
  | NotEqual (left, right) -> fprintf ppf "%a != %a" pp_exp left pp_exp right
  | PostInc e -> fprintf ppf "%a++" pp_exp e
  | PrefInc e -> fprintf ppf "++%a" pp_exp e
  | PostDec e -> fprintf ppf "%a--" pp_exp e
  | PrefDec e -> fprintf ppf "--%a" pp_exp e
  | Null -> fprintf ppf "null"
  | This -> fprintf ppf "this"
  | Super -> fprintf ppf "super"
  | Const v -> fprintf ppf "%a" pp_val v
  | Identifier s -> fprintf ppf "%s" s
  | FieldAccess (left, right) ->
      if is_create left then fprintf ppf "%a.%a" pp_exp_par left pp_exp right
      else fprintf ppf "%a.%a" pp_exp left pp_exp right
  | ArrayAccess (arre, index) -> fprintf ppf "%a[%a]" pp_exp arre pp_exp index
  | ArrayCreateSized (arr_type, size) ->
      fprintf ppf "new %a[%a]" pp_type arr_type pp_exp size
  | ArrayCreateElements (arr_type, expr_list) ->
      fprintf ppf "new %a[] {%a}" pp_type arr_type pp_exp_list expr_list
  | ClassCreate (name, expr_list) ->
      fprintf ppf "new %a(%a)" pp_name name pp_exp_list expr_list
  | CallMethod (m_expr, expr_list) ->
      fprintf ppf "%a(%a)" pp_exp m_expr pp_exp_list expr_list
  | Assign (left, right) -> fprintf ppf "%a = %a" pp_exp left pp_exp right

and pp_exp_par ppf = fprintf ppf "(%a)" pp_exp

and pp_exp_list ppf = pp_print_list ~pp_sep:pp_sep_comma pp_exp ppf

let pp_pairs_dec_l ppf =
  let pp_pair ppf = function
    | Name name, None -> fprintf ppf "%s" name
    | Name name, Some expr -> fprintf ppf "%s = %a" name pp_exp expr
  in
  pp_print_list ~pp_sep:pp_sep_comma pp_pair ppf

let pp_sep_endl ppf () = fprintf ppf "@;<0 0>"

let rec pp_st ppf = function
  | Expression expr -> fprintf ppf "%a;" pp_exp expr
  | If (be_expr, then_st, else_st_o) -> (
      match else_st_o with
      | Some else_st -> (
          match then_st with
          | StmtBlock _ ->
              fprintf ppf "if (%a) %a@;<0 0>else %a" pp_exp be_expr pp_st
                then_st pp_st else_st
          | _ ->
              fprintf ppf "if (%a) %a else %a" pp_exp be_expr pp_st then_st
                pp_st else_st )
      | None -> fprintf ppf "if (%a) %a" pp_exp be_expr pp_st then_st )
  | While (be_expr, body) ->
      fprintf ppf "while (%a) %a" pp_exp be_expr pp_st body
  | For (dec_st_o, be_exp_o, after_el, body) ->
      ( match dec_st_o with
      | Some dec_st -> fprintf ppf "for (%a" pp_st dec_st
      | None -> fprintf ppf "for (;" );
      ( match be_exp_o with
      | Some be_exp -> fprintf ppf " %a;" pp_exp be_exp
      | None -> fprintf ppf ";" );
      fprintf ppf " %a) " pp_exp_list after_el;
      fprintf ppf "%a" pp_st body
  | Break -> fprintf ppf "break;"
  | Continue -> fprintf ppf "continue;"
  | Return expr_o -> (
      match expr_o with
      | Some expr -> fprintf ppf "return %a;" pp_exp expr
      | None -> fprintf ppf "return;" )
  | VarDec (mod_o, var_type, pairs_list) ->
      ( match mod_o with
      | Some modif -> fprintf ppf "%a " pp_modif modif
      | None -> () );
      fprintf ppf "%a " pp_type var_type;
      fprintf ppf "%a;" pp_pairs_dec_l pairs_list
  | StmtBlock st_list ->
      fprintf ppf "@;<0 0>@[<v 3>{@;<0 0>%a@;<0 -3>}@]" pp_st_list st_list

and pp_st_list ppf = pp_print_list ~pp_sep:pp_sep_endl pp_st ppf
