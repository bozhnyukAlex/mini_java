open Opal
open ATD

let parens p = exactly ('(') >> lexeme p >>= fun x -> lexeme (exactly (')')) >> return x 

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

  let rec expression s = 
    choice [
      null;
      super;
      this;
      
    ] s

    

  
  

end
