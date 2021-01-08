  $ (cd ../../../../default && demos/demoParserFirst.exe)
  Class
  ([Public], Name ("Main"), None,
   [([Public; Static],
     Method
     (Void, Name ("main"), [],
      Some (StmtBlock ([VarDec
                        (None, ClassName ("Person"),
                         [(Name ("p"),
                           Some (ClassCreate
                                 (Name ("Person"),
                                  [Const (VInt (80)); Const (VInt (45))])))]);
                        Expression (FieldAccess
                                    (FieldAccess
                                     (Identifier ("System"),
                                      Identifier ("out")),
                                     CallMethod
                                     (Identifier ("println"),
                                      [FieldAccess
                                       (Identifier ("p"),
                                        CallMethod
                                        (Identifier ("getWeight"), []))])));
                        VarDec
                        (None, ClassName ("Child"),
                         [(Name ("ch"),
                           Some (ClassCreate
                                 (Name ("Child"),
                                  [Const (VInt (66)); Const (VInt (20))])))]);
                        Expression (FieldAccess
                                    (Identifier ("ch"),
                                     CallMethod
                                     (Identifier ("setCash"),
                                      [Const (VInt (50))])));
                        Expression (FieldAccess
                                    (Identifier ("ch"),
                                     CallMethod
                                     (Identifier ("giveEvenNumbers100"), [])))]))))])
  Class
  ([], Name ("Person"), None,
   [([Public], VarField (Int, [(Name ("weight"), None)]));
    ([Public], VarField (Int, [(Name ("age"), None)]));
    ([Public],
     Constructor
     (Name ("Person"), [(Int, Name ("w")); (Int, Name ("a"))],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("weight")),
                               Identifier ("w")));
                  Expression (Assign
                              (FieldAccess (This, Identifier ("age")),
                               Identifier ("a")))])));
    ([Public],
     Method
     (Int, Name ("getWeight"), [],
      Some (StmtBlock ([Return (Some (Identifier ("weight")))]))));
    ([Public],
     Method
     (Int, Name ("getAge"), [],
      Some (StmtBlock ([Return (Some (Identifier ("age")))]))));
    ([Public],
     Method
     (Void, Name ("setWeight"), [(Int, Name ("w"))],
      Some (StmtBlock ([Expression (Assign
                                    (FieldAccess (This, Identifier ("weight")),
                                     Identifier ("w")))]))));
    ([Public],
     Method
     (Void, Name ("setAge"), [(Int, Name ("a"))],
      Some (StmtBlock ([Expression (Assign
                                    (FieldAccess (This, Identifier ("age")),
                                     Identifier ("a")))]))))])
  Class
  ([], Name ("Child"), Some (Name ("Person")),
   [([Public], VarField (Int, [(Name ("cash"), None)]));
    ([Public],
     Constructor
     (Name ("Child"), [(Int, Name ("w")); (Int, Name ("a"))],
      StmtBlock ([Expression (CallMethod
                              (Super, [Identifier ("w"); Identifier ("a")]));
                  Expression (Assign (Identifier ("cash"), Const (VInt (0))))])));
    ([Public],
     Method
     (Int, Name ("getCash"), [],
      Some (StmtBlock ([Return (Some (Identifier ("cash")))]))));
    ([Public],
     Method
     (Void, Name ("setCash"), [(Int, Name ("c"))],
      Some (StmtBlock ([Expression (Assign
                                    (FieldAccess (This, Identifier ("cash")),
                                     Identifier ("c")))]))));
    ([Public],
     Constructor
     (Name ("Child"),
      [(Int, Name ("w")); (Int, Name ("a")); (Int, Name ("c"))],
      StmtBlock ([Expression (CallMethod
                              (Super, [Identifier ("w"); Identifier ("a")]));
                  Expression (Assign (Identifier ("cash"), Identifier ("c")))])));
    ([Public],
     Method
     (Void, Name ("giveEvenNumbers100"), [],
      Some (StmtBlock ([For
                        (Some (VarDec
                               (None, Int,
                                [(Name ("i"), Some (Const (VInt (0))))])),
                         Some (Less (Identifier ("i"), Const (VInt (100)))),
                         [PostInc (Identifier ("i"))],
                         StmtBlock ([If
                                     (And
                                      (Equal
                                       (Mod
                                        (Identifier ("i"), Const (VInt (2))),
                                        Const (VInt (0))),
                                       Not (Equal
                                            (Mod
                                             (Identifier ("i"),
                                              Const (VInt (2))),
                                             Const (VInt (1))))),
                                      StmtBlock ([Expression (FieldAccess
                                                              (FieldAccess
                                                               (Identifier ("System"),
                                                                Identifier ("out")),
                                                               CallMethod
                                                               (Identifier ("println"),
                                                                [Identifier ("i")])))]),
                                      Some (StmtBlock ([Continue])))]))]))))])
  $ (cd ../../../../default && demos/demoParserSecond.exe)
  Class
  ([Public], Name ("Main"), None,
   [([Public; Static],
     Method
     (Void, Name ("main"), [],
      Some (StmtBlock ([VarDec
                        (None, Array (ClassName ("Figure")),
                         [(Name ("list"),
                           Some (ArrayCreateElements
                                 (ClassName ("Figure"),
                                  [ClassCreate
                                   (Name ("Circle"), [Const (VInt (5))]);
                                   ClassCreate
                                   (Name ("Rectangle"),
                                    [Const (VInt (2)); Const (VInt (4))]);
                                   ClassCreate (Name ("Triangle"), [])])))]);
                        VarDec
                        (None, ClassName ("AreaVisitor"),
                         [(Name ("areaVisitor"),
                           Some (ClassCreate (Name ("AreaVisitor"), [])))]);
                        VarDec
                        (None, ClassName ("PerimeterVisitor"),
                         [(Name ("perimeterVisitor"),
                           Some (ClassCreate (Name ("PerimeterVisitor"), [])))]);
                        For
                        (Some (VarDec
                               (None, Int,
                                [(Name ("i"), Some (Const (VInt (0))))])),
                         Some (Less
                               (Identifier ("i"),
                                FieldAccess
                                (Identifier ("list"), Identifier ("length")))),
                         [PostInc (Identifier ("i"))],
                         StmtBlock ([Expression (FieldAccess
                                                 (FieldAccess
                                                  (Identifier ("System"),
                                                   Identifier ("out")),
                                                  CallMethod
                                                  (Identifier ("println"),
                                                   [FieldAccess
                                                    (ArrayAccess
                                                     (Identifier ("list"),
                                                      Identifier ("i")),
                                                     CallMethod
                                                     (Identifier ("accept"),
                                                      [Identifier ("areaVisitor")]))])))]));
                        For
                        (Some (VarDec
                               (None, Int,
                                [(Name ("j"), Some (Const (VInt (0))))])),
                         Some (Less
                               (Identifier ("j"),
                                FieldAccess
                                (Identifier ("list"), Identifier ("length")))),
                         [PostInc (Identifier ("j"))],
                         StmtBlock ([Expression (FieldAccess
                                                 (FieldAccess
                                                  (Identifier ("System"),
                                                   Identifier ("out")),
                                                  CallMethod
                                                  (Identifier ("println"),
                                                   [FieldAccess
                                                    (ArrayAccess
                                                     (Identifier ("list"),
                                                      Identifier ("j")),
                                                     CallMethod
                                                     (Identifier ("accept"),
                                                      [Identifier ("perimeterVisitor")]))])))]))]))))])
  Class
  ([Abstract], Name ("Figure"), None,
   [([Abstract],
     Method (Int, Name ("accept"), [(ClassName ("Visitor"), Name ("v"))], None))])
  Class
  ([Abstract], Name ("Visitor"), None,
   [([Abstract],
     Method
     (Int, Name ("visit"), [(ClassName ("Circle"), Name ("circle"))], None));
    ([Abstract],
     Method
     (Int, Name ("visit"), [(ClassName ("Rectangle"), Name ("rectangle"))],
      None));
    ([Abstract],
     Method
     (Int, Name ("visit"), [(ClassName ("Triangle"), Name ("triangle"))], None))])
  Class
  ([], Name ("AreaVisitor"), Some (Name ("Visitor")),
   [([Override],
     Method
     (Int, Name ("visit"), [(ClassName ("Circle"), Name ("circle"))],
      Some (StmtBlock ([Return (Some (Mult
                                      (Mult
                                       (Const (VInt (3)),
                                        FieldAccess
                                        (Identifier ("circle"),
                                         Identifier ("radius"))),
                                       FieldAccess
                                       (Identifier ("circle"),
                                        Identifier ("radius")))))]))));
    ([Override],
     Method
     (Int, Name ("visit"), [(ClassName ("Rectangle"), Name ("rectangle"))],
      Some (StmtBlock ([Return (Some (Mult
                                      (FieldAccess
                                       (Identifier ("rectangle"),
                                        Identifier ("a")),
                                       FieldAccess
                                       (Identifier ("rectangle"),
                                        Identifier ("b")))))]))));
    ([Override],
     Method
     (Int, Name ("visit"), [(ClassName ("Triangle"), Name ("triangle"))],
      Some (StmtBlock ([VarDec
                        (None, Int,
                         [(Name ("p"),
                           Some (Div
                                 (Add
                                  (Add
                                   (FieldAccess
                                    (Identifier ("triangle"), Identifier ("a")),
                                    FieldAccess
                                    (Identifier ("triangle"), Identifier ("b"))),
                                   FieldAccess
                                   (Identifier ("triangle"), Identifier ("c"))),
                                  Const (VInt (2)))))]);
                        Return (Some (Mult
                                      (Mult
                                       (Mult
                                        (Identifier ("p"),
                                         Sub
                                         (Identifier ("p"),
                                          FieldAccess
                                          (Identifier ("triangle"),
                                           Identifier ("a")))),
                                        Sub
                                        (Identifier ("p"),
                                         FieldAccess
                                         (Identifier ("triangle"),
                                          Identifier ("b")))),
                                       Sub
                                       (Identifier ("p"),
                                        FieldAccess
                                        (Identifier ("triangle"),
                                         Identifier ("c"))))))]))))])
  Class
  ([], Name ("PerimeterVisitor"), Some (Name ("Visitor")),
   [([Override],
     Method
     (Int, Name ("visit"), [(ClassName ("Circle"), Name ("circle"))],
      Some (StmtBlock ([Return (Some (Mult
                                      (Mult
                                       (Const (VInt (2)), Const (VInt (3))),
                                       FieldAccess
                                       (Identifier ("circle"),
                                        Identifier ("radius")))))]))));
    ([Override],
     Method
     (Int, Name ("visit"), [(ClassName ("Rectangle"), Name ("rectangle"))],
      Some (StmtBlock ([Return (Some (Mult
                                      (Add
                                       (FieldAccess
                                        (Identifier ("rectangle"),
                                         Identifier ("a")),
                                        FieldAccess
                                        (Identifier ("rectangle"),
                                         Identifier ("b"))),
                                       Const (VInt (2)))))]))));
    ([Override],
     Method
     (Int, Name ("visit"), [(ClassName ("Triangle"), Name ("triangle"))],
      Some (StmtBlock ([Return (Some (Add
                                      (Add
                                       (FieldAccess
                                        (Identifier ("triangle"),
                                         Identifier ("a")),
                                        FieldAccess
                                        (Identifier ("triangle"),
                                         Identifier ("b"))),
                                       FieldAccess
                                       (Identifier ("triangle"),
                                        Identifier ("c")))))]))))])
  Class
  ([], Name ("Circle"), Some (Name ("Figure")),
   [([Public], VarField (Int, [(Name ("radius"), None)]));
    ([Public],
     Constructor
     (Name ("Circle"), [(Int, Name ("radius"))],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("radius")),
                               Identifier ("radius")))])));
    ([Public],
     Constructor
     (Name ("Circle"), [],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("radius")),
                               Const (VInt (1))))])));
    ([Override],
     Method
     (Int, Name ("accept"), [(ClassName ("Visitor"), Name ("v"))],
      Some (StmtBlock ([Return (Some (FieldAccess
                                      (Identifier ("v"),
                                       CallMethod
                                       (Identifier ("visit"), [This]))))]))))])
  Class
  ([], Name ("Triangle"), Some (Name ("Figure")),
   [([Public],
     VarField
     (Int, [(Name ("a"), None); (Name ("b"), None); (Name ("c"), None)]));
    ([Public],
     Constructor
     (Name ("Triangle"),
      [(Int, Name ("a")); (Int, Name ("b")); (Int, Name ("c"))],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("a")),
                               Identifier ("a")));
                  Expression (Assign
                              (FieldAccess (This, Identifier ("b")),
                               Identifier ("b")));
                  Expression (Assign
                              (FieldAccess (This, Identifier ("c")),
                               Identifier ("c")))])));
    ([Public],
     Constructor
     (Name ("Triangle"), [],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("a")),
                               Const (VInt (1))));
                  Expression (Assign
                              (FieldAccess (This, Identifier ("b")),
                               Const (VInt (1))));
                  Expression (Assign
                              (FieldAccess (This, Identifier ("c")),
                               Const (VInt (1))))])));
    ([Override],
     Method
     (Int, Name ("accept"), [(ClassName ("Visitor"), Name ("v"))],
      Some (StmtBlock ([Return (Some (FieldAccess
                                      (Identifier ("v"),
                                       CallMethod
                                       (Identifier ("visit"), [This]))))]))))])
  Class
  ([], Name ("Rectangle"), Some (Name ("Figure")),
   [([Public], VarField (Int, [(Name ("a"), None); (Name ("b"), None)]));
    ([Public],
     Constructor
     (Name ("Rectangle"), [],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("a")),
                               Const (VInt (1))));
                  Expression (Assign
                              (FieldAccess (This, Identifier ("b")),
                               Const (VInt (1))))])));
    ([Public],
     Constructor
     (Name ("Rectangle"), [(Int, Name ("a")); (Int, Name ("b"))],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("a")),
                               Identifier ("a")));
                  Expression (Assign
                              (FieldAccess (This, Identifier ("b")),
                               Identifier ("b")))])));
    ([Override],
     Method
     (Int, Name ("accept"), [(ClassName ("Visitor"), Name ("v"))],
      Some (StmtBlock ([Return (Some (FieldAccess
                                      (Identifier ("v"),
                                       CallMethod
                                       (Identifier ("visit"), [This]))))]))))])










  $ (cd ../../../../default && demos/demoClassLoader.exe)
  -------------------TESTING_INHERITANCE-------------------
  
  [["Child" ->
     { this_key = "Child";
       field_table =
       [["cash" ->
          { f_type = Int; key = "cash"; is_not_mutable = false; sub_tree = None
            }
       
  "weight" ->
   { f_type = Int; key = "weight"; is_not_mutable = false; sub_tree = None }
  
  "age" ->
   { f_type = Int; key = "age"; is_not_mutable = false; sub_tree = None }
  
  ]]
  ;
  method_table =
  [["getWeight@@" ->
     { m_type = Int; is_abstract = false; is_overridable = true;
       has_override_annotation = false; args = []; key = "getWeight@@";
       body = Some (StmtBlock ([Return (Some (Identifier ("weight")))]));
       is_overriden = false }
  
  "getAge@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = true; args = []; key = "getAge@@";
     body =
     Some (StmtBlock ([Return (Some (Add (Identifier ("age"), Const (VInt (1)))))]));
     is_overriden = true }
  
  "setAgeInt@@" ->
   { m_type = Void; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = [(Int, Name ("a"))];
     key = "setAgeInt@@";
     body =
     Some (StmtBlock ([Expression (Assign
                                   (FieldAccess (This, Identifier ("age")),
                                    Identifier ("a")))]));
     is_overriden = false }
  
  "getCash@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "getCash@@";
     body = Some (StmtBlock ([Return (Some (Identifier ("cash")))]));
     is_overriden = false }
  
  "equalsClassName (\"Object\")@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false;
     args = [(ClassName ("Object"), Name ("obj"))];
     key = "equalsClassName (\"Object\")@@";
     body =
     Some (StmtBlock ([If
                       (Equal (This, Identifier ("obj")),
                        Return (Some (Const (VInt (1)))),
                        Some (Return (Some (Const (VInt (0))))))]));
     is_overriden = false }
  
  "setCashInt@@" ->
   { m_type = Void; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = [(Int, Name ("c"))];
     key = "setCashInt@@";
     body =
     Some (StmtBlock ([Expression (Assign
                                   (FieldAccess (This, Identifier ("cash")),
                                    Identifier ("c")))]));
     is_overriden = false }
  
  "toString@@" ->
   { m_type = ClassName ("String"); is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "toString@@";
     body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]));
     is_overriden = false }
  
  "setWeightInt@@" ->
   { m_type = Void; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = [(Int, Name ("w"))];
     key = "setWeightInt@@";
     body =
     Some (StmtBlock ([Expression (Assign
                                   (FieldAccess (This, Identifier ("weight")),
                                    Identifier ("w")))]));
     is_overriden = false }
  
  "giveEvenNumbers100@@" ->
   { m_type = Void; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "giveEvenNumbers100@@";
     body =
     Some (StmtBlock ([For
                       (Some (VarDec
                              (None, Int,
                               [(Name ("i"), Some (Const (VInt (0))))])),
                        Some (Less (Identifier ("i"), Const (VInt (100)))),
                        [PostInc (Identifier ("i"))],
                        StmtBlock ([If
                                    (And
                                     (Equal
                                      (Mod (Identifier ("i"), Const (VInt (2))),
                                       Const (VInt (0))),
                                      Not (Equal
                                           (Mod
                                            (Identifier ("i"),
                                             Const (VInt (2))),
                                            Const (VInt (1))))),
                                     StmtBlock ([Expression (FieldAccess
                                                             (FieldAccess
                                                              (Identifier ("System"),
                                                               Identifier ("out")),
                                                              CallMethod
                                                              (Identifier ("println"),
                                                               [Identifier ("i")])))]),
                                     Some (StmtBlock ([Continue])))]))]));
     is_overriden = false }
  
  ]]
  ;
  constructor_table =
  [["ChildIntIntInt$$" ->
     { key = "ChildIntIntInt$$";
       args = [(Int, Name ("w")); (Int, Name ("a")); (Int, Name ("c"))];
       body =
       StmtBlock ([Expression (CallMethod
                               (Super, [Identifier ("w"); Identifier ("a")]));
                   Expression (Assign (Identifier ("cash"), Identifier ("c")))])
       }
  
  "ChildIntInt$$" ->
   { key = "ChildIntInt$$"; args = [(Int, Name ("w")); (Int, Name ("a"))];
     body =
     StmtBlock ([Expression (CallMethod
                             (Super, [Identifier ("w"); Identifier ("a")]));
                 Expression (Assign (Identifier ("cash"), Const (VInt (0))))])
     }
  
  ]]
  ; children_keys = []; is_abstract = false; is_inheritable = true;
  parent_key = Some ("Person");
  dec_tree =
  Class
  ([], Name ("Child"), Some (Name ("Person")),
   [([Public], VarField (Int, [(Name ("cash"), None)]));
    ([Public],
     Constructor
     (Name ("Child"), [(Int, Name ("w")); (Int, Name ("a"))],
      StmtBlock ([Expression (CallMethod
                              (Super, [Identifier ("w"); Identifier ("a")]));
                  Expression (Assign (Identifier ("cash"), Const (VInt (0))))])));
    ([Override; Public],
     Method
     (Int, Name ("getAge"), [],
      Some (StmtBlock ([Return (Some (Add
                                      (Identifier ("age"), Const (VInt (1)))))]))));
    ([Public],
     Method
     (Int, Name ("getCash"), [],
      Some (StmtBlock ([Return (Some (Identifier ("cash")))]))));
    ([Public],
     Method
     (Void, Name ("setCash"), [(Int, Name ("c"))],
      Some (StmtBlock ([Expression (Assign
                                    (FieldAccess (This, Identifier ("cash")),
                                     Identifier ("c")))]))));
    ([Public],
     Constructor
     (Name ("Child"),
      [(Int, Name ("w")); (Int, Name ("a")); (Int, Name ("c"))],
      StmtBlock ([Expression (CallMethod
                              (Super, [Identifier ("w"); Identifier ("a")]));
                  Expression (Assign (Identifier ("cash"), Identifier ("c")))])));
    ([Public],
     Method
     (Void, Name ("giveEvenNumbers100"), [],
      Some (StmtBlock ([For
                        (Some (VarDec
                               (None, Int,
                                [(Name ("i"), Some (Const (VInt (0))))])),
                         Some (Less (Identifier ("i"), Const (VInt (100)))),
                         [PostInc (Identifier ("i"))],
                         StmtBlock ([If
                                     (And
                                      (Equal
                                       (Mod
                                        (Identifier ("i"), Const (VInt (2))),
                                        Const (VInt (0))),
                                       Not (Equal
                                            (Mod
                                             (Identifier ("i"),
                                              Const (VInt (2))),
                                             Const (VInt (1))))),
                                      StmtBlock ([Expression (FieldAccess
                                                              (FieldAccess
                                                               (Identifier ("System"),
                                                                Identifier ("out")),
                                                               CallMethod
                                                               (Identifier ("println"),
                                                                [Identifier ("i")])))]),
                                      Some (StmtBlock ([Continue])))]))]))))])
  }
  
  "Main" ->
   { this_key = "Main"; field_table = [[]]
                        ;
     method_table =
     [["equalsClassName (\"Object\")@@" ->
        { m_type = Int; is_abstract = false; is_overridable = true;
          has_override_annotation = false;
          args = [(ClassName ("Object"), Name ("obj"))];
          key = "equalsClassName (\"Object\")@@";
          body =
          Some (StmtBlock ([If
                            (Equal (This, Identifier ("obj")),
                             Return (Some (Const (VInt (1)))),
                             Some (Return (Some (Const (VInt (0))))))]));
          is_overriden = false }
     
  "toString@@" ->
   { m_type = ClassName ("String"); is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "toString@@";
     body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]));
     is_overriden = false }
  
  "main@@" ->
   { m_type = Void; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "main@@";
     body =
     Some (StmtBlock ([VarDec
                       (None, ClassName ("Person"),
                        [(Name ("p"),
                          Some (ClassCreate
                                (Name ("Person"),
                                 [Const (VInt (80)); Const (VInt (45))])))])]));
     is_overriden = false }
  
  ]]
  ;
  constructor_table =
  [["Main$$" -> { key = "Main$$"; args = []; body = StmtBlock ([]) }
  
  ]]
  ; children_keys = []; is_abstract = false; is_inheritable = true;
  parent_key = Some ("Object");
  dec_tree =
  Class
  ([Public], Name ("Main"), None,
   [([Public; Static],
     Method
     (Void, Name ("main"), [],
      Some (StmtBlock ([VarDec
                        (None, ClassName ("Person"),
                         [(Name ("p"),
                           Some (ClassCreate
                                 (Name ("Person"),
                                  [Const (VInt (80)); Const (VInt (45))])))])]))))])
  }
  
  "Object" ->
   { this_key = "Object"; field_table = [[]]
                          ;
     method_table =
     [["toString@@" ->
        { m_type = ClassName ("String"); is_abstract = false;
          is_overridable = true; has_override_annotation = false; args = [];
          key = "toString@@";
          body =
          Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]));
          is_overriden = false }
     
  "equalsClassName (\"Object\")@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false;
     args = [(ClassName ("Object"), Name ("obj"))];
     key = "equalsClassName (\"Object\")@@";
     body =
     Some (StmtBlock ([If
                       (Equal (This, Identifier ("obj")),
                        Return (Some (Const (VInt (1)))),
                        Some (Return (Some (Const (VInt (0))))))]));
     is_overriden = false }
  
  ]]
  ;
  constructor_table =
  [["Object$$" -> { key = "Object$$"; args = []; body = StmtBlock ([]) }
  
  ]]
  ; children_keys = ["Main"; "String"; "Person"]; is_abstract = false;
  is_inheritable = true; parent_key = None;
  dec_tree =
  Class
  ([Public], Name ("Object"), None,
   [([Public],
     Method
     (Int, Name ("equals"), [(ClassName ("Object"), Name ("obj"))],
      Some (StmtBlock ([If
                        (Equal (This, Identifier ("obj")),
                         Return (Some (Const (VInt (1)))),
                         Some (Return (Some (Const (VInt (0))))))]))));
    ([Public],
     Method
     (ClassName ("String"), Name ("toString"), [],
      Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]))))])
  }
  
  "String" ->
   { this_key = "String";
     field_table =
     [["value" ->
        { f_type = Array (Char); key = "value"; is_not_mutable = true;
          sub_tree = None }
     
  ]]
  ;
  method_table =
  [["startsWithClassName (\"String\")@@" ->
     { m_type = Bool; is_abstract = false; is_overridable = true;
       has_override_annotation = false;
       args = [(ClassName ("String"), Name ("prefix"))];
       key = "startsWithClassName (\"String\")@@";
       body =
       Some (StmtBlock ([Return (Some (CallMethod
                                       (Identifier ("startsWith"),
                                        [Identifier ("prefix");
                                         Const (VInt (0))])))]));
       is_overriden = false }
  
  "concatClassName (\"String\")@@" ->
   { m_type = ClassName ("String"); is_abstract = false; is_overridable = true;
     has_override_annotation = false;
     args = [(ClassName ("String"), Name ("str"))];
     key = "concatClassName (\"String\")@@";
     body =
     Some (StmtBlock ([VarDec
                       (None, Int,
                        [(Name ("otherLen"),
                          Some (FieldAccess
                                (Identifier ("str"),
                                 CallMethod (Identifier ("length"), []))))]);
                       If
                       (Equal
                        (FieldAccess
                         (Identifier ("str"),
                          CallMethod (Identifier ("length"), [])),
                         Const (VInt (0))),
                        StmtBlock ([Return (Some (This))]), None);
                       VarDec
                       (None, Int,
                        [(Name ("len"),
                          Some (FieldAccess
                                (Identifier ("value"), Identifier ("length"))))]);
                       VarDec
                       (None, Array (Char),
                        [(Name ("newValue"),
                          Some (ArrayCreateSized
                                (Char,
                                 Add
                                 (Identifier ("len"), Identifier ("otherLen")))))]);
                       For
                       (Some (VarDec
                              (None, Int,
                               [(Name ("i"), Some (Const (VInt (0))))])),
                        Some (Less (Identifier ("i"), Identifier ("len"))),
                        [PostInc (Identifier ("i"))],
                        StmtBlock ([Expression (Assign
                                                (ArrayAccess
                                                 (Identifier ("newValue"),
                                                  Identifier ("i")),
                                                 ArrayAccess
                                                 (Identifier ("value"),
                                                  Identifier ("i"))))]));
                       For
                       (Some (VarDec
                              (None, Int,
                               [(Name ("j"), Some (Identifier ("len")))])),
                        Some (Less
                              (Identifier ("j"),
                               Add
                               (Identifier ("len"), Identifier ("otherLen")))),
                        [PostInc (Identifier ("j"))],
                        StmtBlock ([Expression (Assign
                                                (ArrayAccess
                                                 (Identifier ("newValue"),
                                                  Identifier ("j")),
                                                 ArrayAccess
                                                 (FieldAccess
                                                  (Identifier ("str"),
                                                   Identifier ("value")),
                                                  Sub
                                                  (Identifier ("j"),
                                                   Identifier ("len")))))]));
                       Return (Some (ClassCreate
                                     (Name ("String"),
                                      [Identifier ("newValue")])))]));
     is_overriden = false }
  
  "equalsClassName (\"Object\")@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false;
     args = [(ClassName ("Object"), Name ("obj"))];
     key = "equalsClassName (\"Object\")@@";
     body =
     Some (StmtBlock ([If
                       (Equal (This, Identifier ("obj")),
                        Return (Some (Const (VInt (1)))),
                        Some (Return (Some (Const (VInt (0))))))]));
     is_overriden = false }
  
  "toString@@" ->
   { m_type = ClassName ("String"); is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "toString@@";
     body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]));
     is_overriden = false }
  
  "length@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "length@@";
     body =
     Some (StmtBlock ([Return (Some (FieldAccess
                                     (Identifier ("value"),
                                      Identifier ("length"))))]));
     is_overriden = false }
  
  "startsWithClassName (\"String\")Int@@" ->
   { m_type = Bool; is_abstract = false; is_overridable = true;
     has_override_annotation = false;
     args = [(ClassName ("String"), Name ("prefix")); (Int, Name ("toffset"))];
     key = "startsWithClassName (\"String\")Int@@";
     body =
     Some (StmtBlock ([VarDec
                       (None, Array (Char),
                        [(Name ("ta"), Some (Identifier ("value")))]);
                       VarDec
                       (None, Array (Char),
                        [(Name ("pa"),
                          Some (FieldAccess
                                (Identifier ("prefix"), Identifier ("value"))))]);
                       VarDec
                       (None, Int,
                        [(Name ("pc"),
                          Some (FieldAccess
                                (Identifier ("prefix"),
                                 CallMethod (Identifier ("length"), []))))]);
                       If
                       (Or
                        (Less (Identifier ("toffset"), Const (VInt (0))),
                         More
                         (Identifier ("toffset"),
                          Sub
                          (FieldAccess
                           (Identifier ("value"), Identifier ("length")),
                           Identifier ("pc")))),
                        StmtBlock ([Return (Some (Const (VBool (false))))]),
                        None);
                       For
                       (Some (VarDec
                              (None, Int,
                               [(Name ("i"), Some (Identifier ("toffset")))])),
                        Some (Less
                              (Identifier ("i"),
                               Add (Identifier ("toffset"), Identifier ("pc")))),
                        [PostInc (Identifier ("i"))],
                        StmtBlock ([If
                                    (NotEqual
                                     (ArrayAccess
                                      (Identifier ("ta"), Identifier ("i")),
                                      ArrayAccess
                                      (Identifier ("pa"),
                                       Sub
                                       (Identifier ("i"),
                                        Identifier ("toffset")))),
                                     StmtBlock ([Return (Some (Const (VBool (false))))]),
                                     None)]));
                       Return (Some (Const (VBool (true))))]));
     is_overriden = false }
  
  ]]
  ;
  constructor_table =
  [["StringArray (Char)$$" ->
     { key = "StringArray (Char)$$"; args = [(Array (Char), Name ("value"))];
       body =
       StmtBlock ([Expression (Assign
                               (FieldAccess (This, Identifier ("value")),
                                Identifier ("value")))])
       }
  
  "String$$" ->
   { key = "String$$"; args = [];
     body =
     StmtBlock ([Expression (Assign
                             (FieldAccess (This, Identifier ("value")),
                              ArrayCreateSized (Char, Const (VInt (0)))))])
     }
  
  "StringClassName (\"String\")$$" ->
   { key = "StringClassName (\"String\")$$";
     args = [(ClassName ("String"), Name ("original"))];
     body =
     StmtBlock ([Expression (Assign
                             (FieldAccess (This, Identifier ("value")),
                              FieldAccess
                              (Identifier ("original"), Identifier ("value"))))])
     }
  
  ]]
  ; children_keys = []; is_abstract = false; is_inheritable = false;
  parent_key = Some ("Object");
  dec_tree =
  Class
  ([Final], Name ("String"), None,
   [([Public; Final], VarField (Array (Char), [(Name ("value"), None)]));
    ([Public],
     Constructor
     (Name ("String"), [],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("value")),
                               ArrayCreateSized (Char, Const (VInt (0)))))])));
    ([Public],
     Constructor
     (Name ("String"), [(ClassName ("String"), Name ("original"))],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("value")),
                               FieldAccess
                               (Identifier ("original"), Identifier ("value"))))])));
    ([Public],
     Constructor
     (Name ("String"), [(Array (Char), Name ("value"))],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("value")),
                               Identifier ("value")))])));
    ([Public],
     Method
     (Int, Name ("length"), [],
      Some (StmtBlock ([Return (Some (FieldAccess
                                      (Identifier ("value"),
                                       Identifier ("length"))))]))));
    ([Public],
     Method
     (ClassName ("String"), Name ("concat"),
      [(ClassName ("String"), Name ("str"))],
      Some (StmtBlock ([VarDec
                        (None, Int,
                         [(Name ("otherLen"),
                           Some (FieldAccess
                                 (Identifier ("str"),
                                  CallMethod (Identifier ("length"), []))))]);
                        If
                        (Equal
                         (FieldAccess
                          (Identifier ("str"),
                           CallMethod (Identifier ("length"), [])),
                          Const (VInt (0))),
                         StmtBlock ([Return (Some (This))]), None);
                        VarDec
                        (None, Int,
                         [(Name ("len"),
                           Some (FieldAccess
                                 (Identifier ("value"), Identifier ("length"))))]);
                        VarDec
                        (None, Array (Char),
                         [(Name ("newValue"),
                           Some (ArrayCreateSized
                                 (Char,
                                  Add
                                  (Identifier ("len"), Identifier ("otherLen")))))]);
                        For
                        (Some (VarDec
                               (None, Int,
                                [(Name ("i"), Some (Const (VInt (0))))])),
                         Some (Less (Identifier ("i"), Identifier ("len"))),
                         [PostInc (Identifier ("i"))],
                         StmtBlock ([Expression (Assign
                                                 (ArrayAccess
                                                  (Identifier ("newValue"),
                                                   Identifier ("i")),
                                                  ArrayAccess
                                                  (Identifier ("value"),
                                                   Identifier ("i"))))]));
                        For
                        (Some (VarDec
                               (None, Int,
                                [(Name ("j"), Some (Identifier ("len")))])),
                         Some (Less
                               (Identifier ("j"),
                                Add
                                (Identifier ("len"), Identifier ("otherLen")))),
                         [PostInc (Identifier ("j"))],
                         StmtBlock ([Expression (Assign
                                                 (ArrayAccess
                                                  (Identifier ("newValue"),
                                                   Identifier ("j")),
                                                  ArrayAccess
                                                  (FieldAccess
                                                   (Identifier ("str"),
                                                    Identifier ("value")),
                                                   Sub
                                                   (Identifier ("j"),
                                                    Identifier ("len")))))]));
                        Return (Some (ClassCreate
                                      (Name ("String"),
                                       [Identifier ("newValue")])))]))));
    ([Public],
     Method
     (Bool, Name ("startsWith"),
      [(ClassName ("String"), Name ("prefix")); (Int, Name ("toffset"))],
      Some (StmtBlock ([VarDec
                        (None, Array (Char),
                         [(Name ("ta"), Some (Identifier ("value")))]);
                        VarDec
                        (None, Array (Char),
                         [(Name ("pa"),
                           Some (FieldAccess
                                 (Identifier ("prefix"), Identifier ("value"))))]);
                        VarDec
                        (None, Int,
                         [(Name ("pc"),
                           Some (FieldAccess
                                 (Identifier ("prefix"),
                                  CallMethod (Identifier ("length"), []))))]);
                        If
                        (Or
                         (Less (Identifier ("toffset"), Const (VInt (0))),
                          More
                          (Identifier ("toffset"),
                           Sub
                           (FieldAccess
                            (Identifier ("value"), Identifier ("length")),
                            Identifier ("pc")))),
                         StmtBlock ([Return (Some (Const (VBool (false))))]),
                         None);
                        For
                        (Some (VarDec
                               (None, Int,
                                [(Name ("i"), Some (Identifier ("toffset")))])),
                         Some (Less
                               (Identifier ("i"),
                                Add (Identifier ("toffset"), Identifier ("pc")))),
                         [PostInc (Identifier ("i"))],
                         StmtBlock ([If
                                     (NotEqual
                                      (ArrayAccess
                                       (Identifier ("ta"), Identifier ("i")),
                                       ArrayAccess
                                       (Identifier ("pa"),
                                        Sub
                                        (Identifier ("i"),
                                         Identifier ("toffset")))),
                                      StmtBlock ([Return (Some (Const (
                                                                 VBool (false))))]),
                                      None)]));
                        Return (Some (Const (VBool (true))))]))));
    ([Public],
     Method
     (Bool, Name ("startsWith"), [(ClassName ("String"), Name ("prefix"))],
      Some (StmtBlock ([Return (Some (CallMethod
                                      (Identifier ("startsWith"),
                                       [Identifier ("prefix");
                                        Const (VInt (0))])))]))))])
  }
  
  "Person" ->
   { this_key = "Person";
     field_table =
     [["weight" ->
        { f_type = Int; key = "weight"; is_not_mutable = false; sub_tree = None
          }
     
  "age" ->
   { f_type = Int; key = "age"; is_not_mutable = false; sub_tree = None }
  
  ]]
  ;
  method_table =
  [["getWeight@@" ->
     { m_type = Int; is_abstract = false; is_overridable = true;
       has_override_annotation = false; args = []; key = "getWeight@@";
       body = Some (StmtBlock ([Return (Some (Identifier ("weight")))]));
       is_overriden = false }
  
  "getAge@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "getAge@@";
     body = Some (StmtBlock ([Return (Some (Identifier ("age")))]));
     is_overriden = false }
  
  "setAgeInt@@" ->
   { m_type = Void; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = [(Int, Name ("a"))];
     key = "setAgeInt@@";
     body =
     Some (StmtBlock ([Expression (Assign
                                   (FieldAccess (This, Identifier ("age")),
                                    Identifier ("a")))]));
     is_overriden = false }
  
  "equalsClassName (\"Object\")@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false;
     args = [(ClassName ("Object"), Name ("obj"))];
     key = "equalsClassName (\"Object\")@@";
     body =
     Some (StmtBlock ([If
                       (Equal (This, Identifier ("obj")),
                        Return (Some (Const (VInt (1)))),
                        Some (Return (Some (Const (VInt (0))))))]));
     is_overriden = false }
  
  "toString@@" ->
   { m_type = ClassName ("String"); is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "toString@@";
     body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]));
     is_overriden = false }
  
  "setWeightInt@@" ->
   { m_type = Void; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = [(Int, Name ("w"))];
     key = "setWeightInt@@";
     body =
     Some (StmtBlock ([Expression (Assign
                                   (FieldAccess (This, Identifier ("weight")),
                                    Identifier ("w")))]));
     is_overriden = false }
  
  ]]
  ;
  constructor_table =
  [["PersonIntInt$$" ->
     { key = "PersonIntInt$$"; args = [(Int, Name ("w")); (Int, Name ("a"))];
       body =
       StmtBlock ([Expression (Assign
                               (FieldAccess (This, Identifier ("weight")),
                                Identifier ("w")));
                   Expression (Assign
                               (FieldAccess (This, Identifier ("age")),
                                Identifier ("a")))])
       }
  
  ]]
  ; children_keys = ["Child"]; is_abstract = false; is_inheritable = true;
  parent_key = Some ("Object");
  dec_tree =
  Class
  ([], Name ("Person"), None,
   [([Public], VarField (Int, [(Name ("weight"), None)]));
    ([Public], VarField (Int, [(Name ("age"), None)]));
    ([Public],
     Constructor
     (Name ("Person"), [(Int, Name ("w")); (Int, Name ("a"))],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("weight")),
                               Identifier ("w")));
                  Expression (Assign
                              (FieldAccess (This, Identifier ("age")),
                               Identifier ("a")))])));
    ([Public],
     Method
     (Int, Name ("getWeight"), [],
      Some (StmtBlock ([Return (Some (Identifier ("weight")))]))));
    ([Public],
     Method
     (Int, Name ("getAge"), [],
      Some (StmtBlock ([Return (Some (Identifier ("age")))]))));
    ([Public],
     Method
     (Void, Name ("setWeight"), [(Int, Name ("w"))],
      Some (StmtBlock ([Expression (Assign
                                    (FieldAccess (This, Identifier ("weight")),
                                     Identifier ("w")))]))));
    ([Public],
     Method
     (Void, Name ("setAge"), [(Int, Name ("a"))],
      Some (StmtBlock ([Expression (Assign
                                    (FieldAccess (This, Identifier ("age")),
                                     Identifier ("a")))]))))])
  }
  
  -------------------SIMILAR_FIELDS-------------------
  
  Similar fields
  -------------------SIMILAR_METHODS_ERROR-------------------
  
  Method with this type exists
  -------------------SIMILAR_CONSTRUCTOR_ERROR-------------------
  
  Constructor with this type exists
  -------------------ABSTRACTNESS_ERRORS-------------------
  
  Abstract method in non-abstract class
  No body of non-abstract method
  Abstract method cannot have body
  Abstract method must be overriden
  -------------------FINAL_MODIFIERS_ERRORS-------------------
  
  Final class cannot be inherited
  ]]
  [["Child" ->
     { this_key = "Child";
       field_table =
       [["cash" ->
          { f_type = Int; key = "cash"; is_not_mutable = false; sub_tree = None
            }
       
  "weight" ->
   { f_type = Int; key = "weight"; is_not_mutable = false; sub_tree = None }
  
  "age" ->
   { f_type = Int; key = "age"; is_not_mutable = false; sub_tree = None }
  
  ]]
  ;
  method_table =
  [["getWeight@@" ->
     { m_type = Int; is_abstract = false; is_overridable = true;
       has_override_annotation = false; args = []; key = "getWeight@@";
       body = Some (StmtBlock ([Return (Some (Identifier ("weight")))]));
       is_overriden = false }
  
  "getCash@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "getCash@@";
     body = Some (StmtBlock ([Return (Some (Identifier ("cash")))]));
     is_overriden = false }
  
  "equalsClassName (\"Object\")@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false;
     args = [(ClassName ("Object"), Name ("obj"))];
     key = "equalsClassName (\"Object\")@@";
     body =
     Some (StmtBlock ([If
                       (Equal (This, Identifier ("obj")),
                        Return (Some (Const (VInt (1)))),
                        Some (Return (Some (Const (VInt (0))))))]));
     is_overriden = false }
  
  "setCashInt@@" ->
   { m_type = Void; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = [(Int, Name ("c"))];
     key = "setCashInt@@";
     body =
     Some (StmtBlock ([Expression (Assign
                                   (FieldAccess (This, Identifier ("cash")),
                                    Identifier ("c")))]));
     is_overriden = false }
  
  "toString@@" ->
   { m_type = ClassName ("String"); is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "toString@@";
     body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]));
     is_overriden = false }
  
  ]]
  ;
  constructor_table =
  [["ChildIntIntInt$$" ->
     { key = "ChildIntIntInt$$";
       args = [(Int, Name ("w")); (Int, Name ("a")); (Int, Name ("c"))];
       body =
       StmtBlock ([Expression (CallMethod
                               (Super, [Identifier ("w"); Identifier ("a")]));
                   Expression (Assign (Identifier ("cash"), Identifier ("c")))])
       }
  
  "ChildIntInt$$" ->
   { key = "ChildIntInt$$"; args = [(Int, Name ("w")); (Int, Name ("a"))];
     body =
     StmtBlock ([Expression (CallMethod
                             (Super, [Identifier ("w"); Identifier ("a")]));
                 Expression (Assign (Identifier ("cash"), Const (VInt (0))))])
     }
  
  ]]
  ; children_keys = []; is_abstract = false; is_inheritable = true;
  parent_key = Some ("Person");
  dec_tree =
  Class
  ([], Name ("Child"), Some (Name ("Person")),
   [([Public], VarField (Int, [(Name ("cash"), None)]));
    ([Public],
     Constructor
     (Name ("Child"), [(Int, Name ("w")); (Int, Name ("a"))],
      StmtBlock ([Expression (CallMethod
                              (Super, [Identifier ("w"); Identifier ("a")]));
                  Expression (Assign (Identifier ("cash"), Const (VInt (0))))])));
    ([Public],
     Method
     (Int, Name ("getCash"), [],
      Some (StmtBlock ([Return (Some (Identifier ("cash")))]))));
    ([Public],
     Method
     (Void, Name ("setCash"), [(Int, Name ("c"))],
      Some (StmtBlock ([Expression (Assign
                                    (FieldAccess (This, Identifier ("cash")),
                                     Identifier ("c")))]))));
    ([Public],
     Constructor
     (Name ("Child"),
      [(Int, Name ("w")); (Int, Name ("a")); (Int, Name ("c"))],
      StmtBlock ([Expression (CallMethod
                              (Super, [Identifier ("w"); Identifier ("a")]));
                  Expression (Assign (Identifier ("cash"), Identifier ("c")))])))])
  }
  
  "Main" ->
   { this_key = "Main"; field_table = [[]]
                        ;
     method_table =
     [["equalsClassName (\"Object\")@@" ->
        { m_type = Int; is_abstract = false; is_overridable = true;
          has_override_annotation = false;
          args = [(ClassName ("Object"), Name ("obj"))];
          key = "equalsClassName (\"Object\")@@";
          body =
          Some (StmtBlock ([If
                            (Equal (This, Identifier ("obj")),
                             Return (Some (Const (VInt (1)))),
                             Some (Return (Some (Const (VInt (0))))))]));
          is_overriden = false }
     
  "toString@@" ->
   { m_type = ClassName ("String"); is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "toString@@";
     body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]));
     is_overriden = false }
  
  "main@@" ->
   { m_type = Void; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "main@@";
     body =
     Some (StmtBlock ([VarDec
                       (None, ClassName ("Person"),
                        [(Name ("p"),
                          Some (ClassCreate
                                (Name ("Person"),
                                 [Const (VInt (80)); Const (VInt (45))])))])]));
     is_overriden = false }
  
  ]]
  ;
  constructor_table =
  [["Main$$" -> { key = "Main$$"; args = []; body = StmtBlock ([]) }
  
  ]]
  ; children_keys = []; is_abstract = false; is_inheritable = true;
  parent_key = Some ("Object");
  dec_tree =
  Class
  ([Public], Name ("Main"), None,
   [([Public; Static],
     Method
     (Void, Name ("main"), [],
      Some (StmtBlock ([VarDec
                        (None, ClassName ("Person"),
                         [(Name ("p"),
                           Some (ClassCreate
                                 (Name ("Person"),
                                  [Const (VInt (80)); Const (VInt (45))])))])]))))])
  }
  
  "Object" ->
   { this_key = "Object"; field_table = [[]]
                          ;
     method_table =
     [["toString@@" ->
        { m_type = ClassName ("String"); is_abstract = false;
          is_overridable = true; has_override_annotation = false; args = [];
          key = "toString@@";
          body =
          Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]));
          is_overriden = false }
     
  "equalsClassName (\"Object\")@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false;
     args = [(ClassName ("Object"), Name ("obj"))];
     key = "equalsClassName (\"Object\")@@";
     body =
     Some (StmtBlock ([If
                       (Equal (This, Identifier ("obj")),
                        Return (Some (Const (VInt (1)))),
                        Some (Return (Some (Const (VInt (0))))))]));
     is_overriden = false }
  
  ]]
  ;
  constructor_table =
  [["Object$$" -> { key = "Object$$"; args = []; body = StmtBlock ([]) }
  
  ]]
  ; children_keys = ["Main"; "String"; "Person"]; is_abstract = false;
  is_inheritable = true; parent_key = None;
  dec_tree =
  Class
  ([Public], Name ("Object"), None,
   [([Public],
     Method
     (Int, Name ("equals"), [(ClassName ("Object"), Name ("obj"))],
      Some (StmtBlock ([If
                        (Equal (This, Identifier ("obj")),
                         Return (Some (Const (VInt (1)))),
                         Some (Return (Some (Const (VInt (0))))))]))));
    ([Public],
     Method
     (ClassName ("String"), Name ("toString"), [],
      Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]))))])
  }
  
  "String" ->
   { this_key = "String";
     field_table =
     [["value" ->
        { f_type = Array (Char); key = "value"; is_not_mutable = true;
          sub_tree = None }
     
  ]]
  ;
  method_table =
  [["startsWithClassName (\"String\")@@" ->
     { m_type = Bool; is_abstract = false; is_overridable = true;
       has_override_annotation = false;
       args = [(ClassName ("String"), Name ("prefix"))];
       key = "startsWithClassName (\"String\")@@";
       body =
       Some (StmtBlock ([Return (Some (CallMethod
                                       (Identifier ("startsWith"),
                                        [Identifier ("prefix");
                                         Const (VInt (0))])))]));
       is_overriden = false }
  
  "concatClassName (\"String\")@@" ->
   { m_type = ClassName ("String"); is_abstract = false; is_overridable = true;
     has_override_annotation = false;
     args = [(ClassName ("String"), Name ("str"))];
     key = "concatClassName (\"String\")@@";
     body =
     Some (StmtBlock ([VarDec
                       (None, Int,
                        [(Name ("otherLen"),
                          Some (FieldAccess
                                (Identifier ("str"),
                                 CallMethod (Identifier ("length"), []))))]);
                       If
                       (Equal
                        (FieldAccess
                         (Identifier ("str"),
                          CallMethod (Identifier ("length"), [])),
                         Const (VInt (0))),
                        StmtBlock ([Return (Some (This))]), None);
                       VarDec
                       (None, Int,
                        [(Name ("len"),
                          Some (FieldAccess
                                (Identifier ("value"), Identifier ("length"))))]);
                       VarDec
                       (None, Array (Char),
                        [(Name ("newValue"),
                          Some (ArrayCreateSized
                                (Char,
                                 Add
                                 (Identifier ("len"), Identifier ("otherLen")))))]);
                       For
                       (Some (VarDec
                              (None, Int,
                               [(Name ("i"), Some (Const (VInt (0))))])),
                        Some (Less (Identifier ("i"), Identifier ("len"))),
                        [PostInc (Identifier ("i"))],
                        StmtBlock ([Expression (Assign
                                                (ArrayAccess
                                                 (Identifier ("newValue"),
                                                  Identifier ("i")),
                                                 ArrayAccess
                                                 (Identifier ("value"),
                                                  Identifier ("i"))))]));
                       For
                       (Some (VarDec
                              (None, Int,
                               [(Name ("j"), Some (Identifier ("len")))])),
                        Some (Less
                              (Identifier ("j"),
                               Add
                               (Identifier ("len"), Identifier ("otherLen")))),
                        [PostInc (Identifier ("j"))],
                        StmtBlock ([Expression (Assign
                                                (ArrayAccess
                                                 (Identifier ("newValue"),
                                                  Identifier ("j")),
                                                 ArrayAccess
                                                 (FieldAccess
                                                  (Identifier ("str"),
                                                   Identifier ("value")),
                                                  Sub
                                                  (Identifier ("j"),
                                                   Identifier ("len")))))]));
                       Return (Some (ClassCreate
                                     (Name ("String"),
                                      [Identifier ("newValue")])))]));
     is_overriden = false }
  
  "equalsClassName (\"Object\")@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false;
     args = [(ClassName ("Object"), Name ("obj"))];
     key = "equalsClassName (\"Object\")@@";
     body =
     Some (StmtBlock ([If
                       (Equal (This, Identifier ("obj")),
                        Return (Some (Const (VInt (1)))),
                        Some (Return (Some (Const (VInt (0))))))]));
     is_overriden = false }
  
  "toString@@" ->
   { m_type = ClassName ("String"); is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "toString@@";
     body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]));
     is_overriden = false }
  
  "length@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "length@@";
     body =
     Some (StmtBlock ([Return (Some (FieldAccess
                                     (Identifier ("value"),
                                      Identifier ("length"))))]));
     is_overriden = false }
  
  "startsWithClassName (\"String\")Int@@" ->
   { m_type = Bool; is_abstract = false; is_overridable = true;
     has_override_annotation = false;
     args = [(ClassName ("String"), Name ("prefix")); (Int, Name ("toffset"))];
     key = "startsWithClassName (\"String\")Int@@";
     body =
     Some (StmtBlock ([VarDec
                       (None, Array (Char),
                        [(Name ("ta"), Some (Identifier ("value")))]);
                       VarDec
                       (None, Array (Char),
                        [(Name ("pa"),
                          Some (FieldAccess
                                (Identifier ("prefix"), Identifier ("value"))))]);
                       VarDec
                       (None, Int,
                        [(Name ("pc"),
                          Some (FieldAccess
                                (Identifier ("prefix"),
                                 CallMethod (Identifier ("length"), []))))]);
                       If
                       (Or
                        (Less (Identifier ("toffset"), Const (VInt (0))),
                         More
                         (Identifier ("toffset"),
                          Sub
                          (FieldAccess
                           (Identifier ("value"), Identifier ("length")),
                           Identifier ("pc")))),
                        StmtBlock ([Return (Some (Const (VBool (false))))]),
                        None);
                       For
                       (Some (VarDec
                              (None, Int,
                               [(Name ("i"), Some (Identifier ("toffset")))])),
                        Some (Less
                              (Identifier ("i"),
                               Add (Identifier ("toffset"), Identifier ("pc")))),
                        [PostInc (Identifier ("i"))],
                        StmtBlock ([If
                                    (NotEqual
                                     (ArrayAccess
                                      (Identifier ("ta"), Identifier ("i")),
                                      ArrayAccess
                                      (Identifier ("pa"),
                                       Sub
                                       (Identifier ("i"),
                                        Identifier ("toffset")))),
                                     StmtBlock ([Return (Some (Const (VBool (false))))]),
                                     None)]));
                       Return (Some (Const (VBool (true))))]));
     is_overriden = false }
  
  ]]
  ;
  constructor_table =
  [["StringArray (Char)$$" ->
     { key = "StringArray (Char)$$"; args = [(Array (Char), Name ("value"))];
       body =
       StmtBlock ([Expression (Assign
                               (FieldAccess (This, Identifier ("value")),
                                Identifier ("value")))])
       }
  
  "String$$" ->
   { key = "String$$"; args = [];
     body =
     StmtBlock ([Expression (Assign
                             (FieldAccess (This, Identifier ("value")),
                              ArrayCreateSized (Char, Const (VInt (0)))))])
     }
  
  "StringClassName (\"String\")$$" ->
   { key = "StringClassName (\"String\")$$";
     args = [(ClassName ("String"), Name ("original"))];
     body =
     StmtBlock ([Expression (Assign
                             (FieldAccess (This, Identifier ("value")),
                              FieldAccess
                              (Identifier ("original"), Identifier ("value"))))])
     }
  
  ]]
  ; children_keys = []; is_abstract = false; is_inheritable = false;
  parent_key = Some ("Object");
  dec_tree =
  Class
  ([Final], Name ("String"), None,
   [([Public; Final], VarField (Array (Char), [(Name ("value"), None)]));
    ([Public],
     Constructor
     (Name ("String"), [],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("value")),
                               ArrayCreateSized (Char, Const (VInt (0)))))])));
    ([Public],
     Constructor
     (Name ("String"), [(ClassName ("String"), Name ("original"))],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("value")),
                               FieldAccess
                               (Identifier ("original"), Identifier ("value"))))])));
    ([Public],
     Constructor
     (Name ("String"), [(Array (Char), Name ("value"))],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("value")),
                               Identifier ("value")))])));
    ([Public],
     Method
     (Int, Name ("length"), [],
      Some (StmtBlock ([Return (Some (FieldAccess
                                      (Identifier ("value"),
                                       Identifier ("length"))))]))));
    ([Public],
     Method
     (ClassName ("String"), Name ("concat"),
      [(ClassName ("String"), Name ("str"))],
      Some (StmtBlock ([VarDec
                        (None, Int,
                         [(Name ("otherLen"),
                           Some (FieldAccess
                                 (Identifier ("str"),
                                  CallMethod (Identifier ("length"), []))))]);
                        If
                        (Equal
                         (FieldAccess
                          (Identifier ("str"),
                           CallMethod (Identifier ("length"), [])),
                          Const (VInt (0))),
                         StmtBlock ([Return (Some (This))]), None);
                        VarDec
                        (None, Int,
                         [(Name ("len"),
                           Some (FieldAccess
                                 (Identifier ("value"), Identifier ("length"))))]);
                        VarDec
                        (None, Array (Char),
                         [(Name ("newValue"),
                           Some (ArrayCreateSized
                                 (Char,
                                  Add
                                  (Identifier ("len"), Identifier ("otherLen")))))]);
                        For
                        (Some (VarDec
                               (None, Int,
                                [(Name ("i"), Some (Const (VInt (0))))])),
                         Some (Less (Identifier ("i"), Identifier ("len"))),
                         [PostInc (Identifier ("i"))],
                         StmtBlock ([Expression (Assign
                                                 (ArrayAccess
                                                  (Identifier ("newValue"),
                                                   Identifier ("i")),
                                                  ArrayAccess
                                                  (Identifier ("value"),
                                                   Identifier ("i"))))]));
                        For
                        (Some (VarDec
                               (None, Int,
                                [(Name ("j"), Some (Identifier ("len")))])),
                         Some (Less
                               (Identifier ("j"),
                                Add
                                (Identifier ("len"), Identifier ("otherLen")))),
                         [PostInc (Identifier ("j"))],
                         StmtBlock ([Expression (Assign
                                                 (ArrayAccess
                                                  (Identifier ("newValue"),
                                                   Identifier ("j")),
                                                  ArrayAccess
                                                  (FieldAccess
                                                   (Identifier ("str"),
                                                    Identifier ("value")),
                                                   Sub
                                                   (Identifier ("j"),
                                                    Identifier ("len")))))]));
                        Return (Some (ClassCreate
                                      (Name ("String"),
                                       [Identifier ("newValue")])))]))));
    ([Public],
     Method
     (Bool, Name ("startsWith"),
      [(ClassName ("String"), Name ("prefix")); (Int, Name ("toffset"))],
      Some (StmtBlock ([VarDec
                        (None, Array (Char),
                         [(Name ("ta"), Some (Identifier ("value")))]);
                        VarDec
                        (None, Array (Char),
                         [(Name ("pa"),
                           Some (FieldAccess
                                 (Identifier ("prefix"), Identifier ("value"))))]);
                        VarDec
                        (None, Int,
                         [(Name ("pc"),
                           Some (FieldAccess
                                 (Identifier ("prefix"),
                                  CallMethod (Identifier ("length"), []))))]);
                        If
                        (Or
                         (Less (Identifier ("toffset"), Const (VInt (0))),
                          More
                          (Identifier ("toffset"),
                           Sub
                           (FieldAccess
                            (Identifier ("value"), Identifier ("length")),
                            Identifier ("pc")))),
                         StmtBlock ([Return (Some (Const (VBool (false))))]),
                         None);
                        For
                        (Some (VarDec
                               (None, Int,
                                [(Name ("i"), Some (Identifier ("toffset")))])),
                         Some (Less
                               (Identifier ("i"),
                                Add (Identifier ("toffset"), Identifier ("pc")))),
                         [PostInc (Identifier ("i"))],
                         StmtBlock ([If
                                     (NotEqual
                                      (ArrayAccess
                                       (Identifier ("ta"), Identifier ("i")),
                                       ArrayAccess
                                       (Identifier ("pa"),
                                        Sub
                                        (Identifier ("i"),
                                         Identifier ("toffset")))),
                                      StmtBlock ([Return (Some (Const (
                                                                 VBool (false))))]),
                                      None)]));
                        Return (Some (Const (VBool (true))))]))));
    ([Public],
     Method
     (Bool, Name ("startsWith"), [(ClassName ("String"), Name ("prefix"))],
      Some (StmtBlock ([Return (Some (CallMethod
                                      (Identifier ("startsWith"),
                                       [Identifier ("prefix");
                                        Const (VInt (0))])))]))))])
  }
  
  "Person" ->
   { this_key = "Person";
     field_table =
     [["weight" ->
        { f_type = Int; key = "weight"; is_not_mutable = false; sub_tree = None
          }
     
  "age" ->
   { f_type = Int; key = "age"; is_not_mutable = false; sub_tree = None }
  
  ]]
  ;
  method_table =
  [["getWeight@@" ->
     { m_type = Int; is_abstract = false; is_overridable = true;
       has_override_annotation = false; args = []; key = "getWeight@@";
       body = Some (StmtBlock ([Return (Some (Identifier ("weight")))]));
       is_overriden = false }
  
  "getAge@@" ->
   { m_type = Int; is_abstract = false; is_overridable = false;
     has_override_annotation = false; args = []; key = "getAge@@";
     body = Some (StmtBlock ([Return (Some (Identifier ("age")))]));
     is_overriden = false }
  
  "equalsClassName (\"Object\")@@" ->
   { m_type = Int; is_abstract = false; is_overridable = true;
     has_override_annotation = false;
     args = [(ClassName ("Object"), Name ("obj"))];
     key = "equalsClassName (\"Object\")@@";
     body =
     Some (StmtBlock ([If
                       (Equal (This, Identifier ("obj")),
                        Return (Some (Const (VInt (1)))),
                        Some (Return (Some (Const (VInt (0))))))]));
     is_overriden = false }
  
  "toString@@" ->
   { m_type = ClassName ("String"); is_abstract = false; is_overridable = true;
     has_override_annotation = false; args = []; key = "toString@@";
     body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))]));
     is_overriden = false }
  
  ]]
  ;
  constructor_table =
  [["PersonIntInt$$" ->
     { key = "PersonIntInt$$"; args = [(Int, Name ("w")); (Int, Name ("a"))];
       body =
       StmtBlock ([Expression (Assign
                               (FieldAccess (This, Identifier ("weight")),
                                Identifier ("w")));
                   Expression (Assign
                               (FieldAccess (This, Identifier ("age")),
                                Identifier ("a")))])
       }
  
  ]]
  ; children_keys = ["Child"]; is_abstract = false; is_inheritable = true;
  parent_key = Some ("Object");
  dec_tree =
  Class
  ([], Name ("Person"), None,
   [([Public], VarField (Int, [(Name ("age"), None)]));
    ([Public], VarField (Int, [(Name ("weight"), None)]));
    ([Public],
     Constructor
     (Name ("Person"), [(Int, Name ("w")); (Int, Name ("a"))],
      StmtBlock ([Expression (Assign
                              (FieldAccess (This, Identifier ("weight")),
                               Identifier ("w")));
                  Expression (Assign
                              (FieldAccess (This, Identifier ("age")),
                               Identifier ("a")))])));
    ([Public],
     Method
     (Int, Name ("getWeight"), [],
      Some (StmtBlock ([Return (Some (Identifier ("weight")))]))));
    ([Final; Public],
     Method
     (Int, Name ("getAge"), [],
      Some (StmtBlock ([Return (Some (Identifier ("age")))]))))])
  }
  
  -------------------@OVERRIDE_ERRORS-------------------
  
  @Override annotation on not overriden method
  ]]




$ (cd ../../../../default && demos/demoInterpreter.exe)
