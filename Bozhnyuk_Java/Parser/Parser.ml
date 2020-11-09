open Opal
open ATD

let parens = between (exactly '(') (exactly ')')


let brackets = between (exactly '[') (exactly ']')

let braces = between (exactly '{') (exactly '}')

let parens' p = (exactly ('(') >>= fun _ -> p) 
  >>= fun x -> exactly (')') 
  >>= fun _ -> return x
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
      
    ] s
  and numeric a = 
    let rec term s = 
      choice [
          term >>= fun left -> spaces
            >>= fun _ -> exactly ('*') 
            >>= fun _ -> spaces
            >>= fun _ -> factor
            >>= fun right -> return (NumericExpr (Mult (left, right)));
          
          term >>= fun left -> spaces
            >>= fun _ -> exactly ('/') 
            >>= fun _ -> spaces
            >>= fun _ -> factor
            >>= fun right -> return (NumericExpr (Mult (left, right)));  
          factor;
      ] s
    and factor s = 
      choice [
        parens expression;
      ] s
    in choice [

      expression >>= fun left -> spaces 
        >>= fun _ -> exactly ('+')
        >>= fun _ -> spaces
        >>= fun _ -> term
        >>= fun right -> return (NumericExpr (Add (left, right))) ;
      
      expression >>= fun left -> spaces 
        >>= fun _ -> exactly ('+')
        >>= fun _ -> spaces
        >>= fun _ -> term
        >>= fun right -> return (NumericExpr (Add (left, right))) ;
        term
    ] a

end 