  $ (cd ../../../../default && demos/demoFirst.exe)
  Ast.Class
  ([Ast.Public], Ast.Identifier ("Main"), None,
   [Ast.Method
    ([Ast.Public; Ast.Static], Ast.Void, Ast.Identifier ("main"),
     [(Ast.Array (Ast.String), Ast.Identifier ("args"))],
     Some (Ast.StmtBlock ([Ast.VarDec
                           (Ast.ClassName ("Person"),
                            [(Ast.Identifier ("p"),
                              Some (Ast.ClassCreate
                                    ("Person",
                                     [Ast.Const (Ast.VInt (80));
                                      Ast.Const (Ast.VInt (45))])))]);
                           Ast.Expression (Ast.FieldAccess
                                           (Ast.FieldAccess
                                            (Ast.Identifier ("System"),
                                             Ast.Identifier ("out")),
                                            Ast.CallMethod
                                            (Ast.Identifier ("println"),
                                             [Ast.FieldAccess
                                              (Ast.Identifier ("p"),
                                               Ast.CallMethod
                                               (Ast.Identifier ("getWeight"),
                                                []))])));
                           Ast.VarDec
                           (Ast.ClassName ("Child"),
                            [(Ast.Identifier ("ch"),
                              Some (Ast.ClassCreate
                                    ("Child",
                                     [Ast.Const (Ast.VInt (66));
                                      Ast.Const (Ast.VInt (20))])))]);
                           Ast.Expression (Ast.FieldAccess
                                           (Ast.Identifier ("ch"),
                                            Ast.CallMethod
                                            (Ast.Identifier ("setCash"),
                                             [Ast.Const (Ast.VInt (50))])));
                           Ast.Expression (Ast.FieldAccess
                                           (Ast.Identifier ("ch"),
                                            Ast.CallMethod
                                            (Ast.Identifier ("giveEvenNumbers100"),
                                             [])))])))])
  Ast.Class
  ([], Ast.Identifier ("Person"), None,
   [Ast.VarField ([Ast.Public], Ast.Int, [(Ast.Identifier ("weight"), None)]);
    Ast.VarField ([Ast.Public], Ast.Int, [(Ast.Identifier ("age"), None)]);
    Ast.Constructor
    ([Ast.Public], Ast.Identifier ("Person"),
     [(Ast.Int, Ast.Identifier ("w")); (Ast.Int, Ast.Identifier ("a"))],
     Ast.StmtBlock ([Ast.Expression (Ast.Assign
                                     (Ast.FieldAccess
                                      (Ast.This, Ast.Identifier ("weight")),
                                      Ast.Identifier ("w")));
                     Ast.Expression (Ast.Assign
                                     (Ast.FieldAccess
                                      (Ast.This, Ast.Identifier ("age")),
                                      Ast.Identifier ("a")))]));
    Ast.Method
    ([Ast.Public], Ast.Int, Ast.Identifier ("getWeight"), [],
     Some (Ast.StmtBlock ([Ast.Return (Some (Ast.Identifier ("weight")))])));
    Ast.Method
    ([Ast.Public], Ast.Int, Ast.Identifier ("getAge"), [],
     Some (Ast.StmtBlock ([Ast.Return (Some (Ast.Identifier ("age")))])));
    Ast.Method
    ([Ast.Public], Ast.Void, Ast.Identifier ("setWeight"),
     [(Ast.Int, Ast.Identifier ("w"))],
     Some (Ast.StmtBlock ([Ast.Expression (Ast.Assign
                                           (Ast.FieldAccess
                                            (Ast.This,
                                             Ast.Identifier ("weight")),
                                            Ast.Identifier ("w")))])));
    Ast.Method
    ([Ast.Public], Ast.Void, Ast.Identifier ("setAge"),
     [(Ast.Int, Ast.Identifier ("a"))],
     Some (Ast.StmtBlock ([Ast.Expression (Ast.Assign
                                           (Ast.FieldAccess
                                            (Ast.This, Ast.Identifier ("age")),
                                            Ast.Identifier ("a")))])))])
  Ast.Class
  ([], Ast.Identifier ("Child"), Some (Ast.Identifier ("Person")),
   [Ast.VarField ([Ast.Public], Ast.Int, [(Ast.Identifier ("cash"), None)]);
    Ast.Constructor
    ([Ast.Public], Ast.Identifier ("Child"),
     [(Ast.Int, Ast.Identifier ("w")); (Ast.Int, Ast.Identifier ("a"))],
     Ast.StmtBlock ([Ast.Expression (Ast.CallMethod
                                     (Ast.Super,
                                      [Ast.Identifier ("w");
                                       Ast.Identifier ("a")]));
                     Ast.Expression (Ast.Assign
                                     (Ast.Identifier ("cash"),
                                      Ast.Const (Ast.VInt (0))))]));
    Ast.Method
    ([Ast.Public], Ast.Int, Ast.Identifier ("getCash"), [],
     Some (Ast.StmtBlock ([Ast.Return (Some (Ast.Identifier ("cash")))])));
    Ast.Method
    ([Ast.Public], Ast.Void, Ast.Identifier ("setCash"),
     [(Ast.Int, Ast.Identifier ("c"))],
     Some (Ast.StmtBlock ([Ast.Expression (Ast.Assign
                                           (Ast.FieldAccess
                                            (Ast.This, Ast.Identifier ("cash")),
                                            Ast.Identifier ("c")))])));
    Ast.Constructor
    ([Ast.Public], Ast.Identifier ("Child"),
     [(Ast.Int, Ast.Identifier ("w")); (Ast.Int, Ast.Identifier ("a"));
      (Ast.Int, Ast.Identifier ("c"))],
     Ast.StmtBlock ([Ast.Expression (Ast.CallMethod
                                     (Ast.Super,
                                      [Ast.Identifier ("w");
                                       Ast.Identifier ("a")]));
                     Ast.Expression (Ast.Assign
                                     (Ast.Identifier ("cash"),
                                      Ast.Identifier ("c")))]));
    Ast.Method
    ([Ast.Public], Ast.Void, Ast.Identifier ("giveEvenNumbers100"), [],
     Some (Ast.StmtBlock ([Ast.For
                           (Some (Ast.VarDec
                                  (Ast.Int,
                                   [(Ast.Identifier ("i"),
                                     Some (Ast.Const (Ast.VInt (0))))])),
                            Some (Ast.Less
                                  (Ast.Identifier ("i"),
                                   Ast.Const (Ast.VInt (100)))),
                            [Ast.PostInc (Ast.Identifier ("i"))],
                            Ast.StmtBlock ([Ast.If
                                            (Ast.And
                                             (Ast.Equal
                                              (Ast.Mod
                                               (Ast.Identifier ("i"),
                                                Ast.Const (Ast.VInt (2))),
                                               Ast.Const (Ast.VInt (0))),
                                              Ast.Not (Ast.Equal
                                                       (Ast.Mod
                                                        (Ast.Identifier ("i"),
                                                         Ast.Const (Ast.VInt (2))),
                                                        Ast.Const (Ast.VInt (1))))),
                                             Ast.StmtBlock ([Ast.Expression (
                                                              Ast.FieldAccess
                                                              (Ast.FieldAccess
                                                               (Ast.Identifier ("System"),
                                                                Ast.Identifier ("out")),
                                                               Ast.CallMethod
                                                               (Ast.Identifier ("println"),
                                                                [Ast.Identifier ("i")])))]),
                                             Some (Ast.StmtBlock ([Ast.Continue])))]))])))])
