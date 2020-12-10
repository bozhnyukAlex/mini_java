  $ (cd ../../../../default && demos/demoParserFirst.exe)
  Ast.Class
  ([Ast.Public], Ast.Name ("Main"), None,
   [([Ast.Public; Ast.Static],
     Ast.Method
     (Ast.Void, Ast.Name ("main"),
      [(Ast.Array (Ast.String), Ast.Name ("args"))],
      Some (Ast.StmtBlock ([Ast.VarDec
                            (Ast.ClassName ("Person"),
                             [(Ast.Name ("p"),
                               Some (Ast.ClassCreate
                                     (Ast.Name ("Person"),
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
                             [(Ast.Name ("ch"),
                               Some (Ast.ClassCreate
                                     (Ast.Name ("Child"),
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
                                              [])))]))))])
  Ast.Class
  ([], Ast.Name ("Person"), None,
   [([Ast.Public], Ast.VarField (Ast.Int, [(Ast.Name ("weight"), None)]));
    ([Ast.Public], Ast.VarField (Ast.Int, [(Ast.Name ("age"), None)]));
    ([Ast.Public],
     Ast.Constructor
     (Ast.Name ("Person"),
      [(Ast.Int, Ast.Name ("w")); (Ast.Int, Ast.Name ("a"))],
      Ast.StmtBlock ([Ast.Expression (Ast.Assign
                                      (Ast.FieldAccess
                                       (Ast.This, Ast.Identifier ("weight")),
                                       Ast.Identifier ("w")));
                      Ast.Expression (Ast.Assign
                                      (Ast.FieldAccess
                                       (Ast.This, Ast.Identifier ("age")),
                                       Ast.Identifier ("a")))])));
    ([Ast.Public],
     Ast.Method
     (Ast.Int, Ast.Name ("getWeight"), [],
      Some (Ast.StmtBlock ([Ast.Return (Some (Ast.Identifier ("weight")))]))));
    ([Ast.Public],
     Ast.Method
     (Ast.Int, Ast.Name ("getAge"), [],
      Some (Ast.StmtBlock ([Ast.Return (Some (Ast.Identifier ("age")))]))));
    ([Ast.Public],
     Ast.Method
     (Ast.Void, Ast.Name ("setWeight"), [(Ast.Int, Ast.Name ("w"))],
      Some (Ast.StmtBlock ([Ast.Expression (Ast.Assign
                                            (Ast.FieldAccess
                                             (Ast.This,
                                              Ast.Identifier ("weight")),
                                             Ast.Identifier ("w")))]))));
    ([Ast.Public],
     Ast.Method
     (Ast.Void, Ast.Name ("setAge"), [(Ast.Int, Ast.Name ("a"))],
      Some (Ast.StmtBlock ([Ast.Expression (Ast.Assign
                                            (Ast.FieldAccess
                                             (Ast.This, Ast.Identifier ("age")),
                                             Ast.Identifier ("a")))]))))])
  Ast.Class
  ([], Ast.Name ("Child"), Some (Ast.Name ("Person")),
   [([Ast.Public], Ast.VarField (Ast.Int, [(Ast.Name ("cash"), None)]));
    ([Ast.Public],
     Ast.Constructor
     (Ast.Name ("Child"),
      [(Ast.Int, Ast.Name ("w")); (Ast.Int, Ast.Name ("a"))],
      Ast.StmtBlock ([Ast.Expression (Ast.CallMethod
                                      (Ast.Super,
                                       [Ast.Identifier ("w");
                                        Ast.Identifier ("a")]));
                      Ast.Expression (Ast.Assign
                                      (Ast.Identifier ("cash"),
                                       Ast.Const (Ast.VInt (0))))])));
    ([Ast.Public],
     Ast.Method
     (Ast.Int, Ast.Name ("getCash"), [],
      Some (Ast.StmtBlock ([Ast.Return (Some (Ast.Identifier ("cash")))]))));
    ([Ast.Public],
     Ast.Method
     (Ast.Void, Ast.Name ("setCash"), [(Ast.Int, Ast.Name ("c"))],
      Some (Ast.StmtBlock ([Ast.Expression (Ast.Assign
                                            (Ast.FieldAccess
                                             (Ast.This,
                                              Ast.Identifier ("cash")),
                                             Ast.Identifier ("c")))]))));
    ([Ast.Public],
     Ast.Constructor
     (Ast.Name ("Child"),
      [(Ast.Int, Ast.Name ("w")); (Ast.Int, Ast.Name ("a"));
       (Ast.Int, Ast.Name ("c"))],
      Ast.StmtBlock ([Ast.Expression (Ast.CallMethod
                                      (Ast.Super,
                                       [Ast.Identifier ("w");
                                        Ast.Identifier ("a")]));
                      Ast.Expression (Ast.Assign
                                      (Ast.Identifier ("cash"),
                                       Ast.Identifier ("c")))])));
    ([Ast.Public],
     Ast.Method
     (Ast.Void, Ast.Name ("giveEvenNumbers100"), [],
      Some (Ast.StmtBlock ([Ast.For
                            (Some (Ast.VarDec
                                   (Ast.Int,
                                    [(Ast.Name ("i"),
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
                                              Some (Ast.StmtBlock ([Ast.Continue])))]))]))))])
  $ (cd ../../../../default && demos/demoParserSecond.exe)
  Ast.Class
  ([Ast.Public], Ast.Name ("Main"), None,
   [([Ast.Public; Ast.Static],
     Ast.Method
     (Ast.Void, Ast.Name ("main"),
      [(Ast.Array (Ast.String), Ast.Name ("args"))],
      Some (Ast.StmtBlock ([Ast.VarDec
                            (Ast.Array (Ast.ClassName ("Figure")),
                             [(Ast.Name ("list"),
                               Some (Ast.ArrayCreateElements
                                     (Ast.ClassName ("Figure"),
                                      [Ast.ClassCreate
                                       (Ast.Name ("Circle"),
                                        [Ast.Const (Ast.VInt (5))]);
                                       Ast.ClassCreate
                                       (Ast.Name ("Rectangle"),
                                        [Ast.Const (Ast.VInt (2));
                                         Ast.Const (Ast.VInt (4))]);
                                       Ast.ClassCreate
                                       (Ast.Name ("Triangle"), [])])))]);
                            Ast.VarDec
                            (Ast.ClassName ("AreaVisitor"),
                             [(Ast.Name ("areaVisitor"),
                               Some (Ast.ClassCreate
                                     (Ast.Name ("AreaVisitor"), [])))]);
                            Ast.VarDec
                            (Ast.ClassName ("PerimeterVisitor"),
                             [(Ast.Name ("perimeterVisitor"),
                               Some (Ast.ClassCreate
                                     (Ast.Name ("PerimeterVisitor"), [])))]);
                            Ast.For
                            (Some (Ast.VarDec
                                   (Ast.Int,
                                    [(Ast.Name ("i"),
                                      Some (Ast.Const (Ast.VInt (0))))])),
                             Some (Ast.Less
                                   (Ast.Identifier ("i"),
                                    Ast.FieldAccess
                                    (Ast.Identifier ("list"),
                                     Ast.Identifier ("length")))),
                             [Ast.PostInc (Ast.Identifier ("i"))],
                             Ast.StmtBlock ([Ast.Expression (Ast.FieldAccess
                                                             (Ast.FieldAccess
                                                              (Ast.Identifier ("System"),
                                                               Ast.Identifier ("out")),
                                                              Ast.CallMethod
                                                              (Ast.Identifier ("println"),
                                                               [Ast.FieldAccess
                                                                (Ast.ArrayAccess
                                                                 (Ast.Identifier ("list"),
                                                                  Ast.Identifier ("i")),
                                                                 Ast.CallMethod
                                                                 (Ast.Identifier ("accept"),
                                                                  [Ast.Identifier ("areaVisitor")]))])))]));
                            Ast.For
                            (Some (Ast.VarDec
                                   (Ast.Int,
                                    [(Ast.Name ("j"),
                                      Some (Ast.Const (Ast.VInt (0))))])),
                             Some (Ast.Less
                                   (Ast.Identifier ("j"),
                                    Ast.FieldAccess
                                    (Ast.Identifier ("list"),
                                     Ast.Identifier ("length")))),
                             [Ast.PostInc (Ast.Identifier ("j"))],
                             Ast.StmtBlock ([Ast.Expression (Ast.FieldAccess
                                                             (Ast.FieldAccess
                                                              (Ast.Identifier ("System"),
                                                               Ast.Identifier ("out")),
                                                              Ast.CallMethod
                                                              (Ast.Identifier ("println"),
                                                               [Ast.FieldAccess
                                                                (Ast.ArrayAccess
                                                                 (Ast.Identifier ("list"),
                                                                  Ast.Identifier ("j")),
                                                                 Ast.CallMethod
                                                                 (Ast.Identifier ("accept"),
                                                                  [Ast.Identifier ("perimeterVisitor")]))])))]))]))))])
  Ast.Class
  ([Ast.Abstract], Ast.Name ("Figure"), None,
   [([Ast.Abstract],
     Ast.Method
     (Ast.Int, Ast.Name ("accept"),
      [(Ast.ClassName ("Visitor"), Ast.Name ("v"))], None))])
  Ast.Class
  ([Ast.Abstract], Ast.Name ("Visitor"), None,
   [([Ast.Abstract],
     Ast.Method
     (Ast.Int, Ast.Name ("visit"),
      [(Ast.ClassName ("Circle"), Ast.Name ("circle"))], None));
    ([Ast.Abstract],
     Ast.Method
     (Ast.Int, Ast.Name ("visit"),
      [(Ast.ClassName ("Rectangle"), Ast.Name ("rectangle"))], None));
    ([Ast.Abstract],
     Ast.Method
     (Ast.Int, Ast.Name ("visit"),
      [(Ast.ClassName ("Triangle"), Ast.Name ("triangle"))], None))])
  Ast.Class
  ([], Ast.Name ("AreaVisitor"), Some (Ast.Name ("Visitor")),
   [([Ast.Override],
     Ast.Method
     (Ast.Int, Ast.Name ("visit"),
      [(Ast.ClassName ("Circle"), Ast.Name ("circle"))],
      Some (Ast.StmtBlock ([Ast.Return (Some (Ast.Mult
                                              (Ast.Mult
                                               (Ast.Const (Ast.VInt (3)),
                                                Ast.FieldAccess
                                                (Ast.Identifier ("circle"),
                                                 Ast.Identifier ("radius"))),
                                               Ast.FieldAccess
                                               (Ast.Identifier ("circle"),
                                                Ast.Identifier ("radius")))))]))));
    ([Ast.Override],
     Ast.Method
     (Ast.Int, Ast.Name ("visit"),
      [(Ast.ClassName ("Rectangle"), Ast.Name ("rectangle"))],
      Some (Ast.StmtBlock ([Ast.Return (Some (Ast.Mult
                                              (Ast.FieldAccess
                                               (Ast.Identifier ("rectangle"),
                                                Ast.Identifier ("a")),
                                               Ast.FieldAccess
                                               (Ast.Identifier ("rectangle"),
                                                Ast.Identifier ("b")))))]))));
    ([Ast.Override],
     Ast.Method
     (Ast.Int, Ast.Name ("visit"),
      [(Ast.ClassName ("Triangle"), Ast.Name ("triangle"))],
      Some (Ast.StmtBlock ([Ast.VarDec
                            (Ast.Int,
                             [(Ast.Name ("p"),
                               Some (Ast.Div
                                     (Ast.Add
                                      (Ast.Add
                                       (Ast.FieldAccess
                                        (Ast.Identifier ("triangle"),
                                         Ast.Identifier ("a")),
                                        Ast.FieldAccess
                                        (Ast.Identifier ("triangle"),
                                         Ast.Identifier ("b"))),
                                       Ast.FieldAccess
                                       (Ast.Identifier ("triangle"),
                                        Ast.Identifier ("c"))),
                                      Ast.Const (Ast.VInt (2)))))]);
                            Ast.Return (Some (Ast.Mult
                                              (Ast.Mult
                                               (Ast.Mult
                                                (Ast.Identifier ("p"),
                                                 Ast.Sub
                                                 (Ast.Identifier ("p"),
                                                  Ast.FieldAccess
                                                  (Ast.Identifier ("triangle"),
                                                   Ast.Identifier ("a")))),
                                                Ast.Sub
                                                (Ast.Identifier ("p"),
                                                 Ast.FieldAccess
                                                 (Ast.Identifier ("triangle"),
                                                  Ast.Identifier ("b")))),
                                               Ast.Sub
                                               (Ast.Identifier ("p"),
                                                Ast.FieldAccess
                                                (Ast.Identifier ("triangle"),
                                                 Ast.Identifier ("c"))))))]))))])
  Ast.Class
  ([], Ast.Name ("PerimeterVisitor"), Some (Ast.Name ("Visitor")),
   [([Ast.Override],
     Ast.Method
     (Ast.Int, Ast.Name ("visit"),
      [(Ast.ClassName ("Circle"), Ast.Name ("circle"))],
      Some (Ast.StmtBlock ([Ast.Return (Some (Ast.Mult
                                              (Ast.Mult
                                               (Ast.Const (Ast.VInt (2)),
                                                Ast.Const (Ast.VInt (3))),
                                               Ast.FieldAccess
                                               (Ast.Identifier ("circle"),
                                                Ast.Identifier ("radius")))))]))));
    ([Ast.Override],
     Ast.Method
     (Ast.Int, Ast.Name ("visit"),
      [(Ast.ClassName ("Rectangle"), Ast.Name ("rectangle"))],
      Some (Ast.StmtBlock ([Ast.Return (Some (Ast.Mult
                                              (Ast.Add
                                               (Ast.FieldAccess
                                                (Ast.Identifier ("rectangle"),
                                                 Ast.Identifier ("a")),
                                                Ast.FieldAccess
                                                (Ast.Identifier ("rectangle"),
                                                 Ast.Identifier ("b"))),
                                               Ast.Const (Ast.VInt (2)))))]))));
    ([Ast.Override],
     Ast.Method
     (Ast.Int, Ast.Name ("visit"),
      [(Ast.ClassName ("Triangle"), Ast.Name ("triangle"))],
      Some (Ast.StmtBlock ([Ast.Return (Some (Ast.Add
                                              (Ast.Add
                                               (Ast.FieldAccess
                                                (Ast.Identifier ("triangle"),
                                                 Ast.Identifier ("a")),
                                                Ast.FieldAccess
                                                (Ast.Identifier ("triangle"),
                                                 Ast.Identifier ("b"))),
                                               Ast.FieldAccess
                                               (Ast.Identifier ("triangle"),
                                                Ast.Identifier ("c")))))]))))])
  Ast.Class
  ([], Ast.Name ("Circle"), Some (Ast.Name ("Figure")),
   [([Ast.Public], Ast.VarField (Ast.Int, [(Ast.Name ("radius"), None)]));
    ([Ast.Public],
     Ast.Constructor
     (Ast.Name ("Circle"), [(Ast.Int, Ast.Name ("radius"))],
      Ast.StmtBlock ([Ast.Expression (Ast.Assign
                                      (Ast.FieldAccess
                                       (Ast.This, Ast.Identifier ("radius")),
                                       Ast.Identifier ("radius")))])));
    ([Ast.Public],
     Ast.Constructor
     (Ast.Name ("Circle"), [],
      Ast.StmtBlock ([Ast.Expression (Ast.Assign
                                      (Ast.FieldAccess
                                       (Ast.This, Ast.Identifier ("radius")),
                                       Ast.Const (Ast.VInt (1))))])));
    ([Ast.Override],
     Ast.Method
     (Ast.Int, Ast.Name ("accept"),
      [(Ast.ClassName ("Visitor"), Ast.Name ("v"))],
      Some (Ast.StmtBlock ([Ast.Return (Some (Ast.FieldAccess
                                              (Ast.Identifier ("v"),
                                               Ast.CallMethod
                                               (Ast.Identifier ("visit"),
                                                [Ast.This]))))]))))])
  Ast.Class
  ([], Ast.Name ("Triangle"), Some (Ast.Name ("Figure")),
   [([Ast.Public],
     Ast.VarField
     (Ast.Int,
      [(Ast.Name ("a"), None); (Ast.Name ("b"), None); (Ast.Name ("c"), None)]));
    ([Ast.Public],
     Ast.Constructor
     (Ast.Name ("Triangle"),
      [(Ast.Int, Ast.Name ("a")); (Ast.Int, Ast.Name ("b"));
       (Ast.Int, Ast.Name ("c"))],
      Ast.StmtBlock ([Ast.Expression (Ast.Assign
                                      (Ast.FieldAccess
                                       (Ast.This, Ast.Identifier ("a")),
                                       Ast.Identifier ("a")));
                      Ast.Expression (Ast.Assign
                                      (Ast.FieldAccess
                                       (Ast.This, Ast.Identifier ("b")),
                                       Ast.Identifier ("b")));
                      Ast.Expression (Ast.Assign
                                      (Ast.FieldAccess
                                       (Ast.This, Ast.Identifier ("c")),
                                       Ast.Identifier ("c")))])));
    ([Ast.Public],
     Ast.Constructor
     (Ast.Name ("Triangle"), [],
      Ast.StmtBlock ([Ast.Expression (Ast.Assign
                                      (Ast.FieldAccess
                                       (Ast.This, Ast.Identifier ("a")),
                                       Ast.Const (Ast.VInt (1))));
                      Ast.Expression (Ast.Assign
                                      (Ast.FieldAccess
                                       (Ast.This, Ast.Identifier ("b")),
                                       Ast.Const (Ast.VInt (1))));
                      Ast.Expression (Ast.Assign
                                      (Ast.FieldAccess
                                       (Ast.This, Ast.Identifier ("c")),
                                       Ast.Const (Ast.VInt (1))))])));
    ([Ast.Override],
     Ast.Method
     (Ast.Int, Ast.Name ("accept"),
      [(Ast.ClassName ("Visitor"), Ast.Name ("v"))],
      Some (Ast.StmtBlock ([Ast.Return (Some (Ast.FieldAccess
                                              (Ast.Identifier ("v"),
                                               Ast.CallMethod
                                               (Ast.Identifier ("visit"),
                                                [Ast.This]))))]))))])
  Ast.Class
  ([], Ast.Name ("Rectangle"), Some (Ast.Name ("Figure")),
   [([Ast.Public],
     Ast.VarField (Ast.Int, [(Ast.Name ("a"), None); (Ast.Name ("b"), None)]));
    ([Ast.Public],
     Ast.Constructor
     (Ast.Name ("Rectangle"), [],
      Ast.StmtBlock ([Ast.Expression (Ast.Assign
                                      (Ast.FieldAccess
                                       (Ast.This, Ast.Identifier ("a")),
                                       Ast.Const (Ast.VInt (1))));
                      Ast.Expression (Ast.Assign
                                      (Ast.FieldAccess
                                       (Ast.This, Ast.Identifier ("b")),
                                       Ast.Const (Ast.VInt (1))))])));
    ([Ast.Public],
     Ast.Constructor
     (Ast.Name ("Rectangle"),
      [(Ast.Int, Ast.Name ("a")); (Ast.Int, Ast.Name ("b"))],
      Ast.StmtBlock ([Ast.Expression (Ast.Assign
                                      (Ast.FieldAccess
                                       (Ast.This, Ast.Identifier ("a")),
                                       Ast.Identifier ("a")));
                      Ast.Expression (Ast.Assign
                                      (Ast.FieldAccess
                                       (Ast.This, Ast.Identifier ("b")),
                                       Ast.Identifier ("b")))])));
    ([Ast.Override],
     Ast.Method
     (Ast.Int, Ast.Name ("accept"),
      [(Ast.ClassName ("Visitor"), Ast.Name ("v"))],
      Some (Ast.StmtBlock ([Ast.Return (Some (Ast.FieldAccess
                                              (Ast.Identifier ("v"),
                                               Ast.CallMethod
                                               (Ast.Identifier ("visit"),
                                                [Ast.This]))))]))))])


 
