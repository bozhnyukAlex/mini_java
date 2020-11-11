open Opal
open ATD

let parens = between (token "(") (token ")")
let brackets = between (token "[") (token "]") 

let braces = between (token "{") (token "}")

(* let not_space c = match c with
  | ' ' -> true
  | _ -> false *)

(* let digit_c =
  let is_digit ch =
    let c = Char.code ch in
    Char.code '0' <= c && c <= Char.code '9'
  in
  satisfy is_digit *)

(* let rec fix_comb f eta = f (fix_comb f) eta


 let fix_poly : (('a -> 'b) list -> 'a -> 'b) list -> ('a -> 'b) list
= fun l -> fix_comb (fun self l -> List.map (fun li x -> li (self l) x) l) l
 *)

(* let digit = digit_c >>= fun c -> return (Char.code c - Char.code '0') *)

let digits = spaces >> many1 digit => implode
let integer = digits => int_of_string

let get_list_option opt = match opt with Some x -> x | None -> []

module Expr = struct
  open ATD
  let null = token "null" >> return (Null)
  let super = token "super" >> return (Super)
  let this = token "this" >> return (This)

  let constInt = integer >>= fun n -> return (Const (JVInt n))

  let reserved = [
    "true";
    "false";
    "if";
    "else";
    "while";
    "public";
    "final";
    "static";
    "int";
    "boolean";
    "for";
    "null";
    "new";
    "this";
    "super";
  ]
  let ident = (spaces >> letter <~> many alpha_num) => implode >>= function
    | s when List.mem s reserved -> mzero
    | s -> return s

  let identifier = ident => fun s -> Identifier s

  let add_op = token "+" >> return (fun x y -> NumericExpr(Add (x, y)))
  let sub_op = token "-" >> return (fun x y -> NumericExpr(Sub (x, y)))
  let mul_op = token "*" >> return (fun x y -> NumericExpr(Mult (x, y)))
  let div_op = token "/" >> return (fun x y -> NumericExpr(Div (x, y)))
  let mod_op = token "%" >> return (fun x y -> NumericExpr(Mod (x, y)))
  let or_op = token "||" >> return (fun x y -> LogicalExpr(Or (x, y)))
  let and_op = token "&&" >> return (fun x y -> LogicalExpr(And (x, y)))
  let lt_op = token "<" >> return (fun x y -> TestingExpr (Less (x, y)))
  let mt_op = token ">" >> return (fun x y -> TestingExpr (More (x, y)))
  let loet_op = token "<=" >> return (fun x y -> TestingExpr (LessOrEqual (x, y)))
  let moet_op = token ">=" >> return (fun x y -> TestingExpr (MoreOrEqual (x, y)))
  let eq_op = token "==" >> return (fun x y -> TestingExpr (Equal (x, y)))
  let neq_op = token "!=" >> return (fun x y -> TestingExpr (NotEqual (x, y)))



  let atomaric = (ident => fun s -> Identifier s)
              <|> (integer => fun n -> Const (JVInt n))
              <|> (token "false" >> return (Const (JVBool true)))
              <|> (token "true" >> return (Const (JVBool true)))
              <|> (token "false" >> return (Const (JVBool false)))
              <|> null




  let rec expression s =
    choice [
      numeric;
    ] s
    and numeric input = (chainl1 and_expr or_op) input
    and and_expr input = (chainl1 test_expr and_op) input
    and test_expr input = (chainl1 add_expr (lt_op <|> mt_op <|> loet_op <|> moet_op <|> eq_op <|> neq_op)) input
    and add_expr input = (chainl1 mult_expr (add_op <|> sub_op)) input
    and mult_expr input = (chainl1 unary_expr (mul_op <|> div_op <|> mod_op)) input
    and unary_expr input = choice [
      (token "!" >> lexeme primary >>= fun s -> return (LogicalExpr (Not s)));
      (token "-" >> lexeme primary >>= fun x -> return (NumericExpr (Sub (Const (JVInt 0), x))));
      primary;
    ] input
    and primary input = (parens expression <|> field_access <|> arr_access <|> method_call  <|> atomaric) input
    and arr_access input = ((method_call <|> identifier)
                              >>= fun arr_name -> many1 (brackets expression)
                              >>= fun index_list -> return (ArrayAccess (arr_name, index_list))) input

    and field_access input = (((arr_access <|> method_call <|> identifier)
                              >>= fun name -> token "."
                              >> lexeme expression
                              >>= fun f_or_m -> return (FieldAccess (name, f_or_m))) input)

    and expr_sep_by_comma input = sep_by expression (token ",") input
    and method_call input = (identifier
                              >>= fun m_name -> token "("
                              >> expr_sep_by_comma
                              >>= fun expr_list -> token ")"
                              >> return (CallMethod (m_name, expr_list))) input


end
