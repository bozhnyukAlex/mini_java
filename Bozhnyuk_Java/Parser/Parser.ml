open Opal
open ATD

let reserved =
  [
    "true";
    "false";
    "if";
    "else";
    "while";
    "public";
    "final";
    "static";
    "int";
    "String";
    "void";
    "boolean";
    "for";
    "null";
    "new";
    "this";
    "super";
    "class";
    "break";
    "continue";
    "extends";
    "return";
  ]

let parens = between (token "(") (token ")")

let brackets = between (token "[") (token "]")

let digits = spaces >> many1 digit => implode

let integer = digits => int_of_string

let get_list_option opt = match opt with Some x -> x | None -> []

let modifier input = choice 
  [
    (token "public" >> return Public);
    (token "static" >> return Static);
    (token "final" >> return Final);
    (token "abstract" >> return Abstract);
  ] input

module Expr = struct

  let null = token "null" >> return Null

  let%test _ = parse null (LazyStream.of_string "null") = Some Null

  let%test _ = parse null (LazyStream.of_string "   null") = Some Null

  let super = token "super" >> return Super

  let%test _ = parse super (LazyStream.of_string "super") = Some Super

  let%test _ = parse super (LazyStream.of_string "   super") = Some Super

  let this = token "this" >> return This

  let%test _ = parse this (LazyStream.of_string "this") = Some This

  let%test _ = parse this (LazyStream.of_string "   this") = Some This

  let constInt = integer >>= fun n -> return (Const (JVInt n))

  let%test _ = parse constInt (LazyStream.of_string "100500") = Some (Const (JVInt 100500))

  let%test _ = parse constInt (LazyStream.of_string "    100500") = Some (Const (JVInt 100500))

  let constString = 
    let string_of_chars chars = 
      let buf = Buffer.create 16 in
      List.iter (Buffer.add_char buf) chars;
      Buffer.contents buf
    in
      token "\"" >> 
      many (satisfy (fun c -> c <> '\"')) >>= fun list ->
      token "\"" >>
      return (Const(JVString (string_of_chars list)))
      
      
  let%test _ = parse constString (LazyStream.of_string "\"hello world!\"") = Some (Const (JVString "hello world!"))        

  let ident =
    spaces >> letter <~> many alpha_num => implode >>= function
    | s when List.mem s reserved -> mzero
    | s -> return s

  let identifier = ident => fun s -> Identifier s

  let%test _ = parse identifier (LazyStream.of_string "IdentSample") = Some (Identifier "IdentSample")

  let%test _ = parse identifier (LazyStream.of_string "super") = None 

  let%test _ = parse identifier (LazyStream.of_string "123bob") = None 


  let add_op = token "+" >> return (fun x y -> (Add (x, y)))

  let sub_op = token "-" >> return (fun x y -> (Sub (x, y)))

  let mul_op = token "*" >> return (fun x y -> (Mult (x, y)))

  let div_op = token "/" >> return (fun x y -> (Div (x, y)))

  let mod_op = token "%" >> return (fun x y -> (Mod (x, y)))

  let or_op = token "||" >> return (fun x y -> (Or (x, y)))

  let and_op = token "&&" >> return (fun x y -> (And (x, y)))

  let l_op = token "<" >> return (fun x y -> (Less (x, y)))

  let m_op = token ">" >> return (fun x y -> (More (x, y)))

  let le_op =
    token "<=" >> return (fun x y -> (LessOrEqual (x, y)))

  let me_op =
    token ">=" >> return (fun x y -> (MoreOrEqual (x, y)))

  let eq_op = token "==" >> return (fun x y -> (Equal (x, y)))

  let neq_op = token "!=" >> return (fun x y -> (NotEqual (x, y)))

  let atomic =
    identifier <|> constInt <|> constString
    <|> (token "true" >> return (Const (JVBool true)))
    <|> (token "false" >> return (Const (JVBool false)))
    <|> null
  
  let%test _ = parse atomic (LazyStream.of_string "true") = Some (Const (JVBool true))
  
  

  let type_spec_array =
    choice
      [
        token "int"
        >> choice 
          [ 
            (many1 (token "[]") >>
              return (JArray JInt));
            return JInt; 
          ];
        token "String"
        >> choice
            [ 
              (many1 (token "[]") >>
                return (JArray JString)); 
              return JString 
            ];
        token "void" >> return JVoid;
        ( ident >>= fun class_name ->
          choice
            [
              (many1 (token "[]") >>
                return (JArray (JClassName class_name)));
              return (JClassName class_name);
            ] );
      ]

  let%test _ = parse type_spec_array (LazyStream.of_string "int") = Some (JInt)  

  let%test _ = parse type_spec_array (LazyStream.of_string "int[]") = Some (JArray JInt)

  let%test _ = parse type_spec_array (LazyStream.of_string "Car[]") = Some (JArray (JClassName "Car"))

  let type_spec =
    choice
      [
        token "int" >> return JInt;
        token "String" >> return JString;
        token "void" >> return JVoid;
        (ident >>= fun class_name -> return (JClassName class_name));
      ]

  let%test _ = parse type_spec (LazyStream.of_string "int") = Some JInt

  let%test _ = parse type_spec (LazyStream.of_string "   void") = Some JVoid
  
  
  
  

  let rec expression input = choice [ numeric ] input

  and numeric input = (chainl1 and_expr or_op) input

  and and_expr input = (chainl1 test_expr and_op) input

  and test_expr input =
    (chainl1 add_expr
       (le_op <|> me_op <|> l_op <|> m_op <|>  eq_op <|> neq_op))
      input

  and add_expr input = (chainl1 mult_expr (add_op <|> sub_op)) input

  and mult_expr input =
    (chainl1 unary_expr (mul_op <|> div_op <|> mod_op)) input

  and unary_expr input =
    choice
      [
        (token "!" >> lexeme primary >>= fun s -> return (Not s));
        ( token "-" >> lexeme primary >>= fun x ->
          return (Sub (Const (JVInt 0), x)) );
        ( token "++" >> lexeme primary >>= fun x ->
          return  (PrefInc x) );
        ( token "--" >> lexeme primary >>= fun x ->
          return (PrefDec x) );
        ( lexeme primary >>= fun x ->
          token "++" >> return (PostInc x) );
        ( lexeme primary >>= fun x ->
          token "--" >> return (PostDec x) );
        primary;
      ]
      input

  and primary input =
    ( create_obj <|> create_arr <|> assign <|> field_access <|> arr_access <|> method_call <|> this
    <|> super <|> parens expression <|> atomic )
      input

  

  and arr_access input =
    ( this <|> parens (create_arr) <|> super <|> method_call <|> identifier
    >>= fun arr_name ->
      (brackets expression) >>= fun index ->
      return (ArrayAccess (arr_name, index)) )
      input
  
  and field_access input = 
    let fold_arr_field acc el = match el with
      | ArrayAccess (ident, i) -> ArrayAccess (FieldAccess (acc, ident), i)
      | other -> FieldAccess (acc, other) 
    in
      let f_parse = ( this <|> super <|> parens create_obj <|> arr_access <|> method_call <|> identifier)
      in
        (f_parse >>= fun head ->
          many1 (token "." >> f_parse) => fun tl ->
            List.fold_left fold_arr_field head tl) input
          

  and expr_sep_by_comma input = sep_by expression (token ",") input

  and method_call input =
    ( (identifier <|> this <|> super) >>= fun m_name ->
      token "(" >> expr_sep_by_comma >>= fun expr_list ->
      token ")" >> return (CallMethod (m_name, expr_list)) )
      input

  and create_obj input =
    ( token "new" >> ident >>= fun class_name ->
      token "(" >> expr_sep_by_comma >>= fun expr_list ->
      token ")" >> return (ClassCreate (class_name, expr_list))
    )
      input

  and create_arr input =
    ( token "new" >> type_spec >>= fun ts ->
      choice
        [
          (token "[]") >> return (ArrayCreate (ts, None));
          ( (brackets expression) >>= fun size ->
            return  (ArrayCreate (ts, Some size)) );
        ] )
      input

  and assign input = 
    let a_left = field_access <|> arr_access <|> method_call <|> identifier
          in
          ( a_left >>= fun left -> 
          token "=" >>
          expression >>= fun right -> 
          return (Assign (left, right))
          ) input


  let%test _ = parse expression (LazyStream.of_string "a = 2") = Some (Assign (Identifier "a", Const (JVInt 2)))

  let%test _  = parse expression (LazyStream.of_string "a[i] = 2") = Some
                                                          (Assign (ArrayAccess (Identifier "a", Identifier "i"), Const (JVInt 2)))

  let%test _  = parse expression (LazyStream.of_string "a = b = 3") = Some (Assign (Identifier "a", Assign (Identifier "b", Const (JVInt 3))))                                                       
      
  let%test _ = parse expression (LazyStream.of_string "1 + 2") = Some (Add (Const (JVInt 1), Const (JVInt 2)))


  let%test _ = parse expression (LazyStream.of_string "1 + 2 * 3 / 4 % 5 - 6") = Some
                                                  (Sub
                                                    (Add (Const (JVInt 1),
                                                      Mod (Div (Mult (Const (JVInt 2), Const (JVInt 3)), Const (JVInt 4)),
                                                        Const (JVInt 5))),
                                                    Const (JVInt 6)))


  let%test _ = parse expression (LazyStream.of_string "(x + y <= 10) && (a % 2 == 0) || !(c / 2 > 3)") = Some
                                                              (Or
                                                                (And (LessOrEqual (Add (Identifier "x", Identifier "y"), Const (JVInt 10)),
                                                                  Equal (Mod (Identifier "a", Const (JVInt 2)), Const (JVInt 0))),
                                                                Not (More (Div (Identifier "c", Const (JVInt 2)), Const (JVInt 3)))))


                                                                  

  let%test _ = parse expression (LazyStream.of_string "2 + 3 * (5 - 3)") = Some
                                                                (Add (Const (JVInt 2),
                                                                  Mult (Const (JVInt 3), Sub (Const (JVInt 5), Const (JVInt 3)))))
                                                                                       

  let%test _ = parse expression (LazyStream.of_string "a[i]") = Some (ArrayAccess (Identifier "a", Identifier "i"))

  let%test _ = parse expression (LazyStream.of_string "someObj.method(arg1, arg2, arg3)") = Some
                                                                                            (FieldAccess (Identifier "someObj",
                                                                                              CallMethod (Identifier "method",
                                                                                                [Identifier "arg1"; Identifier "arg2"; Identifier "arg3"])))
  

  let%test _ = parse expression (LazyStream.of_string "arr[i].get()") = Some
                                                                          (FieldAccess (ArrayAccess (Identifier "arr", Identifier "i"),
                                                                            CallMethod (Identifier "get", [])))

  let%test _ = parse expression (LazyStream.of_string "a[i].b[j]") = Some
                                                    (ArrayAccess
                                                      (FieldAccess (ArrayAccess (Identifier "a", Identifier "i"),
                                                        Identifier "b"),
                                                      Identifier "j"))
  let%test _ = parse expression (LazyStream.of_string "this.getArray()[i]") = Some
                                              (ArrayAccess (FieldAccess (This, CallMethod (Identifier "getArray", [])),
                                                Identifier "i"))

  let%test _ =  parse expression (LazyStream.of_string "this.getCar().wheels[2].rad") = Some
                                                (FieldAccess
                                                  (ArrayAccess
                                                    (FieldAccess (FieldAccess (This, CallMethod (Identifier "getCar", [])),
                                                      Identifier "wheels"),
                                                    Const (JVInt 2)),
                                                  Identifier "rad"))
  
  let%test _ = parse expression (LazyStream.of_string "a.b[i].c[j]") = Some
                                                  (ArrayAccess
                                                    (FieldAccess
                                                      (ArrayAccess (FieldAccess (Identifier "a", Identifier "b"),
                                                        Identifier "i"),
                                                      Identifier "c"),
                                                    Identifier "j"))


  let%test _ = parse expression (LazyStream.of_string "a.b.c") = Some 
                                                      (FieldAccess (FieldAccess (Identifier "a", Identifier "b"), Identifier "c"))
                                                          
  let%test _ = parse expression (LazyStream.of_string "call(1 + 2, 40)") = Some
                                                                  (CallMethod (Identifier "call",
                                                                    [Add (Const (JVInt 1), Const (JVInt 2)); Const (JVInt 40)]))


  let%test _  = parse expression (LazyStream.of_string "new int[]") = Some (ArrayCreate (JInt, None))

  let%test _ = parse expression (LazyStream.of_string "new int[4]") = Some (ArrayCreate (JInt, Some (Const (JVInt 4))))

  let%test _ = parse expression (LazyStream.of_string "new arr[i]") = Some
                                                        (ArrayCreate (JClassName "arr", Some (Identifier "i")))
 

  let%test _ = parse expression (LazyStream.of_string "new Car(2,\"Ford\")") = Some (ClassCreate ("Car", [Const (JVInt 2); Const (JVString "Ford")]))

  let%test _ = parse expression (LazyStream.of_string "get(new Sth(), new String[10])") = Some
                                                            (CallMethod (Identifier "get",
                                                              [ClassCreate ("Sth", []); ArrayCreate (JString, Some (Const (JVInt 10)))]))

  
  let%test _ = parse expression (LazyStream.of_string "(new Man(3,\"John\")).scream())")  = Some
                                                            (FieldAccess (ClassCreate ("Man", [Const (JVInt 3); Const (JVString "John")]),
                                                              CallMethod (Identifier "scream", [])))



  let%test _ = parse expression (LazyStream.of_string "--(obj.f + (x + y)++)") = Some
                                                                      (PrefDec
                                                                        (Add (FieldAccess (Identifier "obj", Identifier "f"),
                                                                          PostInc (Add (Identifier "x", Identifier "y")))))

                                                   
end


module Stat = struct
  open ATD
  open Expr

  let break_stat = token "break" >> token ";" >> return Break

  let%test _ = parse break_stat (LazyStream.of_string "break;") = Some Break

  let continue_stat = token "continue" >> token ";" >> return Continue

  let%test _ = parse continue_stat (LazyStream.of_string "continue;") = Some Continue

  let return_stat = token "return " >> choice 
    [
      (expression >>= fun ret -> token ";" >> return (Return (Some ret)));
      (token ";" >> return (Return None))
    ] 
                   


  let%test _ = parse return_stat (LazyStream.of_string "return 0;") = Some (Return (Some (Const (JVInt 0))))

  let%test _ = parse return_stat (LazyStream.of_string "return a < b;") = Some (Return (Some
                                                                                  (Less (Identifier "a", Identifier "b"))))

  let expr_stat = expression >>= fun expr -> token ";" >> return (Expression expr)

  let%test _  = parse expr_stat (LazyStream.of_string "fork();") = Some (Expression (CallMethod (Identifier "fork", [])))

  let%test _ = parse expr_stat (LazyStream.of_string "i++;") = Some (Expression (PostInc (Identifier "i")))

  let rec statement input = 
    choice 
      [
        var_declaration;
        break_stat;
        continue_stat;
        return_stat;
        if_stat;
        while_stat;
        for_stat;
        throw_stat;
        expr_stat;
        stat_block;
      ] 
      input
  and if_stat input = 
    (
      token "if" >> 
      token "("  >> 
      expression >>= fun cond_expr -> 
      token ")" >>
      statement >>= fun then_stats ->
      choice [
        (token "else" >> 
        statement >>= fun else_stats ->
        return (If (cond_expr, then_stats, Some else_stats)));
        (return (If (cond_expr, then_stats, None)));
      ]
    ) 
    input

    and stat_block input = (token "{" >> 
                          sep_by statement spaces >>= fun block_stats ->
                          token "}" >>
                          return (StatBlock (block_stats))) 
                          input

    and while_stat input = (token "while" >> 
                          token "(" >>
                          expression >>= fun cond_expr ->
                          token ")" >> 
                          statement >>= fun stat ->
                          return (While (cond_expr, stat))) 
                          input
    and var_declaration = 
      let var_declarator = identifier >>= fun name ->
          (token "=" >>
          expression >>= fun value ->
          return (name, Some value))
          <|> return (name, None)
          in
            many modifier >>= fun modifs -> 
            type_spec_array >>= fun type_specifier ->
            sep_by var_declarator (token ",") >>= fun dec_pairs ->
            token ";" >> return (VarDec (modifs, type_specifier, dec_pairs))
    

    and for_stat input = 
      (
        token "for" >>
        token "(" >>
        choice 
          [
            (statement >>= fun stat -> return (Some stat));
            (token ";" >> return None) 
          ] >>= fun dec -> 
        choice 
          [
            (expression >>= fun expr ->
              token ";" >> return (Some expr));
            (token ";" >> return None);
          ] >>= fun cond ->
        sep_by expression (token ",") >>= fun after ->
        token ")" >>
        statement >>= fun body -> 
        return (For (dec, cond, after, body)) 
      ) 
      input
    and throw_stat input = 
      (
        token "throw" >>
        expression >>= fun expr -> 
        token ";" >>
        return (Throw expr) 
      )
      input
    
    
    let%test _ = parse statement (LazyStream.of_string "public int a = 0, b, c, d = 5;") = Some
                                                              (VarDec ([Public], JInt,
                                                                [(Identifier "a", Some (Const (JVInt 0))); (Identifier "b", None);
                                                                  (Identifier "c", None); (Identifier "d", Some (Const (JVInt 5)))]))
    
    let%test _ = parse statement (LazyStream.of_string "public int[] a = new int[6];") = Some
                                                              (VarDec ([Public], JArray JInt,
                                                                [(Identifier "a", Some (ArrayCreate (JInt, Some (Const (JVInt 6)))))])) 

    let%test _ = parse statement (LazyStream.of_string "public static int a = 0, b = 1, c = 2;") = Some
                                                            (VarDec ([Public; Static], JInt,
                                                              [(Identifier "a", Some (Const (JVInt 0))); (Identifier "b", Some (Const (JVInt 1)));
                                                                (Identifier "c", Some (Const (JVInt 2)))])) 

    
    let%test _ =  parse statement (LazyStream.of_string "if (x < 10) x++;") = Some
                                                            (If (Less (Identifier "x", Const (JVInt 10)),
                                                              Expression (PostInc (Identifier "x")), None))

    
    
    let%test _ = parse statement (LazyStream.of_string "if (a < b) {\n return b - a; \n } else { \n return a - b; \n }") = 
                                                        Some
                                                          (If (Less (Identifier "a", Identifier "b"),
                                                            StatBlock [Return (Some (Sub (Identifier "b", Identifier "a")))],
                                                            Some
                                                              (StatBlock [Return (Some (Sub (Identifier "a", Identifier "b")))])))
    
    let%test _  = parse statement (LazyStream.of_string "array = new int[3];") = Some
                                                        (Expression
                                                          (Assign (Identifier "array",
                                                            ArrayCreate (JInt, Some (Const (JVInt 3))))))


    let%test _ = parse statement (LazyStream.of_string "if (a % 2 == 0 && b < 2) {\n a++;\n b--;\n return a * b; \n } else if (!(b / 2 != 5)) { \n --b; \n  return (a + b)*3; \n } else continue;") = 
                                                        Some
                                                          (If
                                                            (And (Equal (Mod (Identifier "a", Const (JVInt 2)), Const (JVInt 0)),
                                                              Less (Identifier "b", Const (JVInt 2))),
                                                            StatBlock
                                                              [Expression (PostInc (Identifier "a"));
                                                              Expression (PostDec (Identifier "b"));
                                                              Return (Some (Mult (Identifier "a", Identifier "b")))],
                                                            Some
                                                              (If
                                                                (Not (NotEqual (Div (Identifier "b", Const (JVInt 2)), Const (JVInt 5))),
                                                                StatBlock
                                                                [Expression (PrefDec (Identifier "b"));
                                                                  Return (Some (Mult (Add (Identifier "a", Identifier "b"), Const (JVInt 3))))],
                                                                Some Continue))))



    let%test _ = parse statement (LazyStream.of_string "while (d * d <= n) { if (n % d == 0) { return true; } d++; }") = Some
                                                          (While (LessOrEqual (Mult (Identifier "d", Identifier "d"), Identifier "n"),
                                                            StatBlock
                                                              [If (Equal (Mod (Identifier "n", Identifier "d"), Const (JVInt 0)),
                                                                StatBlock [Return (Some (Const (JVBool true)))], None);
                                                              Expression (PostInc (Identifier "d"))]))

    let%test _ = parse statement (LazyStream.of_string "for (int i = 0, j = n - 1; i < j; i++, j--) { System.out.println(\"test\"); }") = Some
                                                                            (For
                                                                              (Some
                                                                                (VarDec ([], JInt,
                                                                                  [(Identifier "i", Some (Const (JVInt 0)));
                                                                                    (Identifier "j", Some (Sub (Identifier "n", Const (JVInt 1))))])),
                                                                              Some (Less (Identifier "i", Identifier "j")),
                                                                              [PostInc (Identifier "i"); PostDec (Identifier "j")],
                                                                              StatBlock
                                                                                [Expression
                                                                                  (FieldAccess (FieldAccess (Identifier "System", Identifier "out"),
                                                                                    CallMethod (Identifier "println", [Const (JVString "test")])))]))
    
    let%test _ = parse statement (LazyStream.of_string "if (somethingWrong()) throw new Exception();") = Some
                                                                            (If (CallMethod (Identifier "somethingWrong", []),
                                                                              Throw (ClassCreate ("Exception", [])), None))                                               
end


let method_declaration input = 
  (
    let param = Expr.type_spec_array >>= fun type_par -> 
      Expr.identifier >>= fun id_par ->
      return (type_par, id_par)
    in
      many modifier >>= fun modifiers ->
      Expr.type_spec_array >>= fun m_type ->
      Expr.identifier >>= fun m_name -> 
      token "(" >>
      sep_by param (token ",") >>= fun param_list ->
      token ")" >>
      choice 
        [
          (Stat.stat_block >>= fun st_block ->
          return (Method (modifiers, m_type, m_name, param_list, Some st_block)));
          (token ";" >> 
          return (Method (modifiers, m_type, m_name, param_list, None)));
        ] 
  ) 
  input

let%test _ = parse method_declaration (LazyStream.of_string "public int arraySum (int[] a) { int sum = 0; for (int i = 0; i < a.length(); i++) {sum = sum + a[i];} return sum; }") = Some
                                                                        (Method ([Public], JInt, Identifier "arraySum",
                                                                          [(JArray JInt, Identifier "a")],
                                                                          Some
                                                                            (StatBlock
                                                                              [VarDec ([], JInt, [(Identifier "sum", Some (Const (JVInt 0)))]);
                                                                              For
                                                                                (Some (VarDec ([], JInt, [(Identifier "i", Some (Const (JVInt 0)))])),
                                                                                Some
                                                                                (Less (Identifier "i",
                                                                                  FieldAccess (Identifier "a", CallMethod (Identifier "length", [])))),
                                                                                [PostInc (Identifier "i")],
                                                                                StatBlock
                                                                                [Expression
                                                                                  (Assign (Identifier "sum",
                                                                                    Add (Identifier "sum",
                                                                                      ArrayAccess (Identifier "a", Identifier "i"))))]);
                                                                              Return (Some (Identifier "sum"))])))

let constructor_declaration input = 
  (
    let param = Expr.type_spec_array >>= fun type_par -> 
      Expr.identifier >>= fun id_par ->
      return (type_par, id_par)
    in
      many modifier >>= fun modifiers ->
      Expr.identifier >>= fun c_name -> 
      token "(" >>
      sep_by param (token ",") >>= fun param_list ->
      token ")" >>
      Stat.stat_block >>= fun c_block ->
      return (Constructor (modifiers, c_name, param_list, c_block))
  )
  input

  let%test _ = parse constructor_declaration (LazyStream.of_string "public Car(int speed, int[] wheels) {this.speed = speed; this.wheels = wheels;}") = Some
                                                                  (Constructor ([Public], Identifier "Car",
                                                                    [(JInt, Identifier "speed"); (JArray JInt, Identifier "wheels")],
                                                                    StatBlock
                                                                      [Expression
                                                                        (Assign (FieldAccess (This, Identifier "speed"), Identifier "speed"));
                                                                      Expression
                                                                        (Assign (FieldAccess (This, Identifier "wheels"), Identifier "wheels"))]))

let field_declaration input = 
  (
    Stat.var_declaration >>= fun var_dec ->
    return (ATD.VarField (var_dec))
  )
  input

let%test _ = parse field_declaration (LazyStream.of_string "public int wheel;") = Some (VarField (VarDec ([Public], JInt, [(Identifier "wheel", None)])))

let class_elem = field_declaration <|> constructor_declaration <|> method_declaration

let class_declaration input = 
  (
    many modifier >>= fun modifiers -> 
    token "class" >>
    Expr.identifier >>= fun class_name ->
    choice 
      [
        (token "extends" >>
        Expr.identifier >>= fun parent_name ->
        return (Some parent_name));
        (return (None));
      ] >>= fun extension -> 
    token "{" >>
    sep_by class_elem spaces >>= fun class_elements ->
    token "}" >>
    return (Class (modifiers, class_name, extension, class_elements))
  )
  input

let parser = many class_declaration

(*
BIG TEST

public class Main
{
	public static void main(String[] args) {
		Person p = new Person(80, 45);
		System.out.println(p.getWeight());
		
		Child ch = new Child(66, 20);
		ch.setCash(50);
		ch.giveEvenNumbers100();
	    
	}
}

class Person {
    public int weight;
    public int age;
    
    public Person(int w, int a) {
        this.weight = w;
        this.age = a;
    }
    
    
    
    public int getWeight() {
        return weight;
    }
    
    public int getAge() {
        return age;
    }
    
    public void setWeight(int w) {
        this.weight = w;
    }
    public void setAge(int a) {
        this.age = a;
    }
    
}

class Child extends Person{
    public int cash;
    
    public Child(int w, int a) {
        super(w,a);
        cash = 0;
    }
    
    public int getCash() {
        return cash;
    }
    
    public void setCash(int c) {
        this.cash = c;
    }
    
    public Child (int w, int a, int c) {
        super(w, a);
        cash = c;
    }
    
    public void giveEvenNumbers100() {
        for (int i = 0; i < 100; i++) {
            if (i % 2 == 0 && !(i % 2 == 1)) {
                System.out.println(i);
            }
            else {
                continue;
            }
        }
    }
    
}

*)


let%test _ = parse parser (LazyStream.of_string "public class Main{public static void main(String[] args) {Person p = new Person(80, 45);System.out.println(p.getWeight());Child ch = new Child(66, 20);ch.setCash(50);ch.giveEvenNumbers100();    }}class Person {    public int weight;    public int age;        public Person(int w, int a) {        this.weight = w;        this.age = a;    }            public int getWeight() {        return weight;    }        public int getAge() {        return age;    }        public void setWeight(int w) {        this.weight = w;    }    public void setAge(int a) {        this.age = a;    }    }class Child extends Person{    public int cash;        public Child(int w, int a) {        super(w,a);        cash = 0;    }        public int getCash() {        return cash;    }        public void setCash(int c) {        this.cash = c;    }        public Child (int w, int a, int c) {        super(w, a);        cash = c;    }        public void giveEvenNumbers100() {for (int i = 0; i < 100; i++) {    if (i % 2 == 0 && !(i % 2 == 1)) {System.out.println(i);    }    else {continue;    }}    }            }") = 
      Some
 [Class ([Public], Identifier "Main", None,
   [Method ([Public; Static], JVoid, Identifier "main",
     [(JArray JString, Identifier "args")],
     Some
      (StatBlock
        [VarDec ([], JClassName "Person",
          [(Identifier "p",
            Some (ClassCreate ("Person", [Const (JVInt 80); Const (JVInt 45)])))]);
         Expression
          (FieldAccess (FieldAccess (Identifier "System", Identifier "out"),
            CallMethod (Identifier "println",
             [FieldAccess (Identifier "p",
               CallMethod (Identifier "getWeight", []))])));
         VarDec ([], JClassName "Child",
          [(Identifier "ch",
            Some (ClassCreate ("Child", [Const (JVInt 66); Const (JVInt 20)])))]);
         Expression
          (FieldAccess (Identifier "ch",
            CallMethod (Identifier "setCash", [Const (JVInt 50)])));
         Expression
          (FieldAccess (Identifier "ch",
            CallMethod (Identifier "giveEvenNumbers100", [])))]))]);
            
  Class ([], Identifier "Person", None,
   [VarField (VarDec ([Public], JInt, [(Identifier "weight", None)]));
    VarField (VarDec ([Public], JInt, [(Identifier "age", None)]));
    Constructor ([Public], Identifier "Person",
     [(JInt, Identifier "w"); (JInt, Identifier "a")],
     StatBlock
      [Expression
        (Assign (FieldAccess (This, Identifier "weight"), Identifier "w"));
       Expression
        (Assign (FieldAccess (This, Identifier "age"), Identifier "a"))]);
    Method ([Public], JInt, Identifier "getWeight", [],
     Some (StatBlock [Return (Some (Identifier "weight"))]));
    Method ([Public], JInt, Identifier "getAge", [],
     Some (StatBlock [Return (Some (Identifier "age"))]));
    Method ([Public], JVoid, Identifier "setWeight", [(JInt, Identifier "w")],
     Some
      (StatBlock
        [Expression
          (Assign (FieldAccess (This, Identifier "weight"), Identifier "w"))]));
    Method ([Public], JVoid, Identifier "setAge", [(JInt, Identifier "a")],
     Some
      (StatBlock
        [Expression
          (Assign (FieldAccess (This, Identifier "age"), Identifier "a"))]))]);

  Class ([], Identifier "Child", Some (Identifier "Person"),
   [VarField (VarDec ([Public], JInt, [(Identifier "cash", None)]));
    Constructor ([Public], Identifier "Child",
     [(JInt, Identifier "w"); (JInt, Identifier "a")],
     StatBlock
      [Expression (CallMethod (Super, [Identifier "w"; Identifier "a"]));
       Expression (Assign (Identifier "cash", Const (JVInt 0)))]);
    Method ([Public], JInt, Identifier "getCash", [],
     Some (StatBlock [Return (Some (Identifier "cash"))]));
    Method ([Public], JVoid, Identifier "setCash", [(JInt, Identifier "c")],
     Some
      (StatBlock
        [Expression
          (Assign (FieldAccess (This, Identifier "cash"), Identifier "c"))]));
    Constructor ([Public], Identifier "Child",
     [(JInt, Identifier "w"); (JInt, Identifier "a"); (JInt, Identifier "c")],
     StatBlock
      [Expression (CallMethod (Super, [Identifier "w"; Identifier "a"]));
       Expression (Assign (Identifier "cash", Identifier "c"))]);
    Method ([Public], JVoid, Identifier "giveEvenNumbers100", [],
     Some
      (StatBlock
        [For
          (Some (VarDec ([], JInt, [(Identifier "i", Some (Const (JVInt 0)))])),
          Some (Less (Identifier "i", Const (JVInt 100))),
          [PostInc (Identifier "i")],
          StatBlock
           [If
             (And
               (Equal (Mod (Identifier "i", Const (JVInt 2)), Const (JVInt 0)),
               Not
                (Equal (Mod (Identifier "i", Const (JVInt 2)), Const (JVInt 1)))),
             StatBlock
              [Expression
                (FieldAccess
                  (FieldAccess (Identifier "System", Identifier "out"),
                  CallMethod (Identifier "println", [Identifier "i"])))],
             Some (StatBlock [Continue]))])]))])]        
