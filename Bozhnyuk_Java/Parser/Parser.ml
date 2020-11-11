open Opal
open ATD

let parens = between (token "(") (token ")")
let brackets = between (token "[") (token "]")
let not_space c = match c with
  | ' ' -> true
  | _ -> false

let digit_c =        
  let is_digit ch =
    let c = Char.code ch in           
    Char.code '0' <= c && c <= Char.code '9'
  in
  satisfy is_digit
  
let rec fix_comb f eta = f (fix_comb f) eta


 let fix_poly : (('a -> 'b) list -> 'a -> 'b) list -> ('a -> 'b) list
= fun l -> fix_comb (fun self l -> List.map (fun li x -> li (self l) x) l) l


(* let digit = digit_c >>= fun c -> return (Char.code c - Char.code '0') *)

let digits = spaces >> many1 digit => implode 
let integer = digits => int_of_string

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

  ]
  let ident = (spaces >> letter <~> many alpha_num) => implode >>= function
    | s when List.mem s reserved -> mzero
    | s -> return s

  let addOp = token "+" >> return (fun x y -> NumericExpr(Add (x, y)))
  let subOp = token "-" >> return (fun x y -> NumericExpr(Sub (x, y)))
  let mulOp = token "*" >> return (fun x y -> NumericExpr(Mult (x, y)))
  let divOp = token "/" >> return (fun x y -> NumericExpr(Div (x, y)))
  let modOp = token "%" >> return (fun x y -> NumericExpr(Mod (x, y)))
  let orOP = token "||" >> return (fun x y -> LogicalExpr(Or (x, y)))
  let andOP = token "&&" >> return (fun x y -> LogicalExpr(And (x, y)))
  let ltOp = token "<" >> return (fun x y -> TestingExpr (Less (x, y)))
  let mtOp = token ">" >> return (fun x y -> TestingExpr (More (x, y)))
  let loetOp = token "<=" >> return (fun x y -> TestingExpr (LessOrEqual (x, y)))
  let moetOp = token ">=" >> return (fun x y -> TestingExpr (MoreOrEqual (x, y)))
  let eqOp = token "==" >> return (fun x y -> TestingExpr (Equal (x, y)))
  let neqOp = token "!=" >> return (fun x y -> TestingExpr (NotEqual (x, y)))



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
    and numeric input = (chainl1 and_expr orOP) input
    and and_expr input = (chainl1 test_expr andOP) input
    and test_expr input = (chainl1 add_expr (ltOp <|> mtOp <|> loetOp <|> moetOp <|> eqOp <|> neqOp)) input
    and add_expr input = (chainl1 mult_expr (addOp <|> subOp)) input
    and mult_expr input = (chainl1 unary_expr (mulOp <|> divOp <|> modOp)) input
    and unary_expr input = choice [
      (token "!" >> lexeme primary >>= fun s -> return (LogicalExpr (Not s)));
      (token "-" >> lexeme primary >>= fun x -> return (NumericExpr (Sub (Const (JVInt 0), x))));
      primary;
    ] input 
    and primary input = (parens expression <|> arr_access <|> field_access <|> atomaric) input
    and arr_access input = ((ident => fun s -> Identifier s) >>= fun arr_name -> brackets expression
                              >>= fun index -> return (ArrayAccess (arr_name, index))) input
    and field_access input = ((ident => fun s -> Identifier s) 
                              >>= fun name -> token "."
                              >> lexeme expression
                              >>= fun f_or_m -> return (Access (name, f_or_m))) input
    
                               
    

  
  


end
