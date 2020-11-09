open Opal
open ATD

let parens = between (exactly '(') (exactly ')')


(* let braces = between (exactly '{') (exactly '}') *)


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
(*
let fix_poly fl: ('a t list -> 'a t) list -> 'a t = 
    fix (fun self -> List.map self fl)

let [even;odd] = 
  let open_even [even;odd] = fun n -> n = 0  || odd (n-1)
  and open_odd  [even;odd] = fun n -> n <> 0 && even (n-1)
 in fix_poly [open_even;open_odd] *)



let digit = digit_c >>= fun c -> return (Char.code c - Char.code '0')

module Expr = struct 
  open ATD
  let null = token "null" >> return (Null) 
  let super = token "super" >> return (Super)

  let this = token "this" >> return (This)

  




  
  let rec expression s = 
    choice [
      parens expression;
      null;
      super;
      this;
      numeric;
      logical;
      testing;
      (expression >>= fun arr_name -> lexeme (exactly '[') 
        >> lexeme expression >>= fun el_index -> lexeme (exactly ']') 
        >> return (ArrayAccess (arr_name, el_index)));
      (expression >>= fun name -> lexeme (exactly '.') >> lexeme expression 
        >>= fun field_or_method -> return (Access (name, field_or_method)));
      
    ] s
  and numeric a = 
    let rec term s = 
      choice [
          (term >>= fun left -> lexeme (exactly ('*')) 
            >> lexeme factor >>= fun right -> return (NumericExpr (Mult (left, right))));

          (term >>= fun left -> lexeme (exactly ('/')) 
            >> lexeme factor >>= fun right -> return (NumericExpr (Div (left, right))));
          factor;
      ] s
    and factor s = 
      choice [
        parens expression;
        exactly ('-') >> lexeme factor 
          >>= fun fact -> return (NumericExpr (Sub (Const (JVInt 0), fact)))
      ] s
    in choice [
      (expression >>= fun left -> lexeme (exactly ('+')) 
        >> lexeme term >>= fun right -> return (NumericExpr (Add (left, right))));
      
      (expression >>= fun left -> lexeme (exactly ('-')) 
        >> lexeme term >>= fun right -> return (NumericExpr (Sub (left, right))));
        term;
    ] a
  and logical a =
    let rec term s = 
      choice [
        (term >>= fun left -> token "&&" >> lexeme factor 
          >>= fun right -> return (LogicalExpr (And (left, right))));
        factor;  
      ] s
    and factor s = 
      choice [
        parens expression;
        exactly ('!') >> lexeme factor 
          >>= fun fact -> return (LogicalExpr (Not fact));
      ] s
    
    in choice [
      (expression >>= fun left -> token "||" >>
        lexeme term >>= fun right -> return (LogicalExpr (Or (left, right)))) ;
      term;
    ] a
  and testing a = 
    choice [
      (expression >>= fun left -> lexeme (exactly ('>')) >> lexeme expression >>=
        fun right -> return (TestingExpr (More (left, right))));

      (expression >>= fun left -> lexeme (exactly ('<')) >> lexeme expression >>=
        fun right -> return (TestingExpr (Less (left, right))));

      (expression >>= fun left -> token ">=" >> lexeme expression >>=
        fun right -> return (TestingExpr (MoreOrEqual (left, right))));  

      (expression >>= fun left -> token "<=" >> lexeme expression >>=
        fun right -> return (TestingExpr (LessOrEqual (left, right))));  
      
      (expression >>= fun left -> token "==" >> lexeme expression >>=
        fun right -> return (TestingExpr (Equal (left, right))));
      
      (expression >>= fun left -> token "!=" >> lexeme expression >>=
        fun right -> return (TestingExpr (MoreOrEqual (left, right))));  
    ] a
    

end 