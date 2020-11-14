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

module Expr = struct
  open ATD

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
(* 
  let constString = token "\"" >> (many (satisfy (fun ch -> ch <> '"'))) >>= fun  *)

  let ident =
    spaces >> letter <~> many alpha_num => implode >>= function
    | s when List.mem s reserved -> mzero
    | s -> return s

  let identifier = ident => fun s -> Identifier s

  let%test _ = parse identifier (LazyStream.of_string "IdentSample") = Some (Identifier "IdentSample")

  let%test _ = parse identifier (LazyStream.of_string "super") = None (*ключевые слова пропускать не должен*)

  let%test _ = parse identifier (LazyStream.of_string "123bob") = None 


  let add_op = token "+" >> return (fun x y -> NumericExpr (Add (x, y)))

  let sub_op = token "-" >> return (fun x y -> NumericExpr (Sub (x, y)))

  let mul_op = token "*" >> return (fun x y -> NumericExpr (Mult (x, y)))

  let div_op = token "/" >> return (fun x y -> NumericExpr (Div (x, y)))

  let mod_op = token "%" >> return (fun x y -> NumericExpr (Mod (x, y)))

  let or_op = token "||" >> return (fun x y -> LogicalExpr (Or (x, y)))

  let and_op = token "&&" >> return (fun x y -> LogicalExpr (And (x, y)))

  let lt_op = token "<" >> return (fun x y -> TestingExpr (Less (x, y)))

  let mt_op = token ">" >> return (fun x y -> TestingExpr (More (x, y)))

  let loet_op =
    token "<=" >> return (fun x y -> TestingExpr (LessOrEqual (x, y)))

  let moet_op =
    token ">=" >> return (fun x y -> TestingExpr (MoreOrEqual (x, y)))

  let eq_op = token "==" >> return (fun x y -> TestingExpr (Equal (x, y)))

  let neq_op = token "!=" >> return (fun x y -> TestingExpr (NotEqual (x, y)))

  let atomaric =
    identifier <|> constInt
    <|> (token "true" >> return (Const (JVBool true)))
    <|> (token "false" >> return (Const (JVBool false)))
    <|> null
  
  let%test _ = parse atomaric (LazyStream.of_string "true") = Some (Const (JVBool true))
  
  

  let type_spec_array =
    choice
      [
        token "int"
        >> choice [ many1 (token "[]") >> return (JArray JInt); return JInt ]; (*для того, чтобы посчитать размерность массива, можно посчитать количество []*)
        token "String"
        >> choice
             [ many1 (token "[]") >> return (JArray JString); return JString ];
        token "void" >> return JVoid;
        ( ident >>= fun class_name ->
          choice
            [
              many1 (token "[]") >> return (JArray (JRef class_name));
              return (JRef class_name);
            ] );
      ]

  let%test _ = parse type_spec_array (LazyStream.of_string "int") = Some (JInt)  

  let%test _ = parse type_spec_array (LazyStream.of_string "int[][][]") = Some (JArray JInt)

  let%test _ = parse type_spec_array (LazyStream.of_string "Car[][][]") = Some (JArray (JRef "Car"))

  let type_spec =
    choice
      [
        token "int" >> return JInt;
        token "String" >> return JString;
        token "void" >> return JVoid;
        (ident >>= fun class_name -> return (JRef class_name));
      ]

  let%test _ = parse type_spec (LazyStream.of_string "int") = Some JInt

  let%test _ = parse type_spec (LazyStream.of_string "   void") = Some JVoid
  
  
  
  

  let rec expression input = choice [ numeric ] input

  and numeric input = (chainl1 and_expr or_op) input

  and and_expr input = (chainl1 test_expr and_op) input

  and test_expr input =
    (chainl1 add_expr
       (loet_op <|> moet_op <|> lt_op <|> mt_op <|>  eq_op <|> neq_op))
      input

  and add_expr input = (chainl1 mult_expr (add_op <|> sub_op)) input

  and mult_expr input =
    (chainl1 unary_expr (mul_op <|> div_op <|> mod_op)) input

  and unary_expr input =
    choice
      [
        (token "!" >> lexeme primary >>= fun s -> return (LogicalExpr (Not s)));
        ( token "-" >> lexeme primary >>= fun x ->
          return (NumericExpr (Sub (Const (JVInt 0), x))) );
        ( token "++" >> lexeme primary >>= fun x ->
          return (NumericExpr (PrefAdd x)) );
        ( token "--" >> lexeme primary >>= fun x ->
          return (NumericExpr (PrefSub x)) );
        ( lexeme primary >>= fun x ->
          token "++" >> return (NumericExpr (PostAdd x)) );
        ( lexeme primary >>= fun x ->
          token "--" >> return (NumericExpr (PostSub x)) );
        primary;
      ]
      input

  and primary input =
    ( create_obj <|> create_arr <|> field_access <|> arr_access <|> this
    <|> super <|> method_call <|> parens expression <|> atomaric )
      input

  and arr_access input =
    ( this <|> parens (create_arr) <|> super <|> method_call <|> identifier
    >>= fun arr_name ->
      many1 (brackets expression) >>= fun index_list ->
      return (ArrayAccess (arr_name, index_list)) )
      input

  and field_access input =
    ( this <|> super <|> parens create_obj <|> arr_access <|> method_call
      <|> identifier
    >>= fun name ->
      token "." >> choice [ field_access; method_call; identifier ] >>= fun f_or_m ->
      return (FieldAccess (name, f_or_m)) )
      input

  and expr_sep_by_comma input = sep_by expression (token ",") input

  and method_call input =
    ( identifier >>= fun m_name ->
      token "(" >> expr_sep_by_comma >>= fun expr_list ->
      token ")" >> return (CallMethod (m_name, expr_list)) )
      input

  and create_obj input =
    ( token "new" >> ident >>= fun class_name ->
      token "(" >> expr_sep_by_comma >>= fun expr_list ->
      token ")" >> return (CreatingExpr (ClassCreate (class_name, expr_list)))
    )
      input

  and create_arr input =
    ( token "new" >> type_spec >>= fun ts ->
      choice
        [
          many1 (token "[]") >> return (CreatingExpr (ArrayCreate (ts, [])));
          ( many1 (brackets expression) >>= fun expr_list ->
            return (CreatingExpr (ArrayCreate (ts, expr_list))) );
        ] )
      input

      
  let%test _ = parse expression (LazyStream.of_string "1 + 2") = Some (NumericExpr (Add (Const (JVInt 1), Const (JVInt 2)))) 

  let%test _ = parse expression (LazyStream.of_string "1 + 2 * 3 / 4 % 5 - 6") = Some (NumericExpr
                                                                                        (Sub
                                                                                          (NumericExpr
                                                                                            (Add (Const (JVInt 1),
                                                                                              NumericExpr
                                                                                                (Mod
                                                                                                  (NumericExpr
                                                                                                    (Div (NumericExpr (Mult (Const (JVInt 2), Const (JVInt 3))),
                                                                                                      Const (JVInt 4))),
                                                                                                  Const (JVInt 5))))),
                                                                                          Const (JVInt 6))))

  let%test _ = parse expression (LazyStream.of_string "(x + y <= 10) && (a % 2 == 0) || !(c / 2 > 3)") = Some
                                                                                                          (LogicalExpr
                                                                                                            (Or
                                                                                                              (LogicalExpr
                                                                                                                (And
                                                                                                                  (TestingExpr
                                                                                                                    (LessOrEqual (NumericExpr (Add (Identifier "x", Identifier "y")),
                                                                                                                      Const (JVInt 10))),
                                                                                                                  TestingExpr
                                                                                                                    (Equal (NumericExpr (Mod (Identifier "a", Const (JVInt 2))),
                                                                                                                      Const (JVInt 0))))),
                                                                                                              LogicalExpr
                                                                                                                (Not
                                                                                                                  (TestingExpr
                                                                                                                    (More (NumericExpr (Div (Identifier "c", Const (JVInt 2))),
                                                                                                                      Const (JVInt 3))))))))

                                                                  

  let%test _ = parse expression (LazyStream.of_string "2 + 3 * (5 - 3)") = Some
                                                                              (NumericExpr
                                                                                (Add (Const (JVInt 2),
                                                                                  NumericExpr
                                                                                    (Mult (Const (JVInt 3),
                                                                                      NumericExpr (Sub (Const (JVInt 5), Const (JVInt 3))))))))                                                                                        

  let%test _ = parse expression (LazyStream.of_string "a[i]") = Some (ArrayAccess (Identifier "a", [Identifier "i"]))

  let%test _ = parse expression (LazyStream.of_string "someObj.method(arg1, arg2, arg3)") = Some
                                                                                            (FieldAccess (Identifier "someObj",
                                                                                              CallMethod (Identifier "method",
                                                                                                [Identifier "arg1"; Identifier "arg2"; Identifier "arg3"])))
  

  let%test _ = parse expression (LazyStream.of_string "arr[i].get()") = Some
                                                                          (FieldAccess (ArrayAccess (Identifier "arr", [Identifier "i"]),
                                                                            CallMethod (Identifier "get", [])))

 
  let%test _ = parse expression (LazyStream.of_string "a.b.c") = Some 
                                                                  (FieldAccess (Identifier "a", FieldAccess (Identifier "b", Identifier "c")))
                                                          
  let%test _ = parse expression (LazyStream.of_string "call(1 + 2, 40)") = Some
                                                                          (CallMethod (Identifier "call",
                                                                            [NumericExpr (Add (Const (JVInt 1), Const (JVInt 2))); Const (JVInt 40)]))

  let%test _  = parse expression (LazyStream.of_string "new int[][][]") = Some (CreatingExpr (ArrayCreate (JInt, [])))

  let%test _ = parse expression (LazyStream.of_string "new int[4][5]") = Some (CreatingExpr (ArrayCreate (JInt, [Const (JVInt 4); Const (JVInt 5)])))

  let%test _ = parse expression (LazyStream.of_string "new arr[2][3][i]") = Some (CreatingExpr 
                                                                                    (ArrayCreate (JRef "arr", 
                                                                                    [Const (JVInt 2); Const (JVInt 3); Identifier "i"])))  

  let%test _ = parse expression (LazyStream.of_string "new Car(2,4)") = Some (CreatingExpr (ClassCreate ("Car", [Const (JVInt 2); Const (JVInt 4)])))

  let%test _ = parse expression (LazyStream.of_string "get(new Sth(), new String[10])") = Some
                                                                                              (CallMethod (Identifier "get",
                                                                                                [CreatingExpr (ClassCreate ("Sth", []));
                                                                                                CreatingExpr (ArrayCreate (JString, [Const (JVInt 10)]))]))
  
  let%test _ = parse expression (LazyStream.of_string "(new Man(3,4)).scream())")  = Some
                                                                                        (FieldAccess
                                                                                          (CreatingExpr (ClassCreate ("Man", [Const (JVInt 3); Const (JVInt 4)])),
                                                                                          CallMethod (Identifier "scream", [])))


  let%test _ = parse expression (LazyStream.of_string "--(obj.f + (x + y)++)") = Some
                                                                                    (NumericExpr
                                                                                      (PrefSub
                                                                                        (NumericExpr
                                                                                          (Add (FieldAccess (Identifier "obj", Identifier "f"),
                                                                                            NumericExpr
                                                                                              (PostAdd (NumericExpr (Add (Identifier "x", Identifier "y")))))))))

                                                   
end


module Stat = struct
  open ATD
  open Expr

  let break_stat = token "break" >> token ";" >> return Break

  let%test _ = parse break_stat (LazyStream.of_string "break;") = Some Break

  let continue_stat = token "continue" >> token ";" >> return Continue

  let%test _ = parse continue_stat (LazyStream.of_string "continue;") = Some Continue

  let return_stat = token "return " >> expression >>= fun ret -> token ";" >> return (Return ret)

  let%test _ = parse return_stat (LazyStream.of_string "return 0;") = Some (Return (Const (JVInt 0)))

  let%test _ = parse return_stat (LazyStream.of_string "return a < b;") = Some (Return 
                                                                                (TestingExpr 
                                                                                  (Less (Identifier "a", Identifier "b"))))

  let expr_stat = expression >>= fun expr -> token ";" >> return (Expression expr)

  let%test _  = parse expr_stat (LazyStream.of_string "fork();") = Some (Expression (CallMethod (Identifier "fork", [])))

  let%test _ = parse expr_stat (LazyStream.of_string "i++;") = Some (Expression (NumericExpr (PostAdd (Identifier "i"))))

  let rec statement input = 
    choice 
      [
        break_stat;
        continue_stat;
        return_stat;
        if_stat;
        while_stat;
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
    
    let%test _ =  parse statement (LazyStream.of_string "if (x < 10) x++;") = Some
                                                            (If (TestingExpr (Less (Identifier "x", Const (JVInt 10))),
                                                              Expression (NumericExpr (PostAdd (Identifier "x"))), None))

    
    
    let%test _ = parse statement (LazyStream.of_string "if (a < b) {\n return b - a; \n } else { \n return a - b; \n }") = 
                                                        Some
                                                          (If (TestingExpr (Less (Identifier "a", Identifier "b")),
                                                            StatBlock [Return (NumericExpr (Sub (Identifier "b", Identifier "a")))],
                                                            Some
                                                              (StatBlock [Return (NumericExpr (Sub (Identifier "a", Identifier "b")))])))


    let%test _ = parse statement (LazyStream.of_string "if (a % 2 == 0 && b < 2) {\n a++;\n b--;\n return a * b; \n } else if (!(b / 2 != 5)) { \n --b; \n  return (a + b)*3; \n } else continue;") = 
                                                        Some
                                                          (If
                                                            (LogicalExpr
                                                              (And
                                                                (TestingExpr
                                                                  (Equal (NumericExpr (Mod (Identifier "a", Const (JVInt 2))),
                                                                    Const (JVInt 0))),
                                                                TestingExpr (Less (Identifier "b", Const (JVInt 2))))),
                                                            StatBlock
                                                              [Expression (NumericExpr (PostAdd (Identifier "a")));
                                                              Expression (NumericExpr (PostSub (Identifier "b")));
                                                              Return (NumericExpr (Mult (Identifier "a", Identifier "b")))],
                                                            Some
                                                              (If
                                                                (LogicalExpr
                                                                  (Not
                                                                    (TestingExpr
                                                                      (NotEqual (NumericExpr (Div (Identifier "b", Const (JVInt 2))),
                                                                        Const (JVInt 5))))),
                                                                StatBlock
                                                                [Expression (NumericExpr (PrefSub (Identifier "b")));
                                                                  Return
                                                                  (NumericExpr
                                                                    (Mult (NumericExpr (Add (Identifier "a", Identifier "b")),
                                                                      Const (JVInt 3))))],
                                                                Some Continue))))


    let%test _ = parse statement (LazyStream.of_string "while (d * d <= n) { if (n % d == 0) { return true; } d++; }") = Some
                                                                          (While
                                                                            (TestingExpr
                                                                              (LessOrEqual (NumericExpr (Mult (Identifier "d", Identifier "d")),
                                                                                Identifier "n")),
                                                                            StatBlock
                                                                              [If
                                                                                (TestingExpr
                                                                                  (Equal (NumericExpr (Mod (Identifier "n", Identifier "d")),
                                                                                    Const (JVInt 0))),
                                                                                StatBlock [Return (Const (JVBool true))], None);
                                                                              Expression (NumericExpr (PostAdd (Identifier "d")))]))

                                                        
end