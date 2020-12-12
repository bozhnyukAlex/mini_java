  $ (cd ../../../../default && demos/demoParserFirst.exe)
  Class
  ([Public], Name ("Main"), None,
   [([Public; Static],
     Method
     (Void, Name ("main"), [(Array (String), Name ("args"))],
      Some (StmtBlock ([VarDec
                        (ClassName ("Person"),
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
                        (ClassName ("Child"),
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
                               (Int, [(Name ("i"), Some (Const (VInt (0))))])),
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
     (Void, Name ("main"), [(Array (String), Name ("args"))],
      Some (StmtBlock ([VarDec
                        (Array (ClassName ("Figure")),
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
                        (ClassName ("AreaVisitor"),
                         [(Name ("areaVisitor"),
                           Some (ClassCreate (Name ("AreaVisitor"), [])))]);
                        VarDec
                        (ClassName ("PerimeterVisitor"),
                         [(Name ("perimeterVisitor"),
                           Some (ClassCreate (Name ("PerimeterVisitor"), [])))]);
                        For
                        (Some (VarDec
                               (Int, [(Name ("i"), Some (Const (VInt (0))))])),
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
                               (Int, [(Name ("j"), Some (Const (VInt (0))))])),
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
                        (Int,
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
  
  [[Child -> { this_key : Child; field_table : [[cash -> { f_type = Int; key = "cash"; is_mutable = false; f_value = None;
    sub_tree = None }
  weight -> { f_type = Int; key = "weight"; is_mutable = false; f_value = None;
    sub_tree = None }
  age -> { f_type = Int; key = "age"; is_mutable = false; f_value = None;
    sub_tree = None }
  ]]; method_table : [[setWeightInt -> { m_type = Void; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = [(Int, Name ("w"))];
    key = "setWeightInt";
    body =
    Some (StmtBlock ([Expression (Assign
                                  (FieldAccess (This, Identifier ("weight")),
                                   Identifier ("w")))]))
    }
  getAge -> { m_type = Int; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = []; key = "getAge";
    body = Some (StmtBlock ([Return (Some (Identifier ("age")))])) }
  toString -> { m_type = String; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = []; key = "toString";
    body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))])) }
  setCashInt -> { m_type = Void; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = [(Int, Name ("c"))];
    key = "setCashInt";
    body =
    Some (StmtBlock ([Expression (Assign
                                  (FieldAccess (This, Identifier ("cash")),
                                   Identifier ("c")))]))
    }
  getCash -> { m_type = Int; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = []; key = "getCash";
    body = Some (StmtBlock ([Return (Some (Identifier ("cash")))])) }
  equals -> { m_type = Int; is_abstract = false; is_overridable = true;
    has_override_annotation = false;
    args = [(ClassName ("Object"), Name ("obj"))]; key = "equals";
    body =
    Some (StmtBlock ([If
                      (Equal (This, Identifier ("obj")),
                       Return (Some (Const (VInt (1)))),
                       Some (Return (Some (Const (VInt (0))))))]))
    }
  setAgeInt -> { m_type = Void; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = [(Int, Name ("a"))];
    key = "setAgeInt";
    body =
    Some (StmtBlock ([Expression (Assign
                                  (FieldAccess (This, Identifier ("age")),
                                   Identifier ("a")))]))
    }
  giveEvenNumbers100 -> { m_type = Void; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = []; key = "giveEvenNumbers100";
    body =
    Some (StmtBlock ([For
                      (Some (VarDec
                             (Int, [(Name ("i"), Some (Const (VInt (0))))])),
                       Some (Less (Identifier ("i"), Const (VInt (100)))),
                       [PostInc (Identifier ("i"))],
                       StmtBlock ([If
                                   (And
                                    (Equal
                                     (Mod (Identifier ("i"), Const (VInt (2))),
                                      Const (VInt (0))),
                                     Not (Equal
                                          (Mod
                                           (Identifier ("i"), Const (VInt (2))),
                                           Const (VInt (1))))),
                                    StmtBlock ([Expression (FieldAccess
                                                            (FieldAccess
                                                             (Identifier ("System"),
                                                              Identifier ("out")),
                                                             CallMethod
                                                             (Identifier ("println"),
                                                              [Identifier ("i")])))]),
                                    Some (StmtBlock ([Continue])))]))]))
    }
  getWeight -> { m_type = Int; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = []; key = "getWeight";
    body = Some (StmtBlock ([Return (Some (Identifier ("weight")))])) }
  ]]; constructor_table : [[ChildIntInt -> { args = [(Int, Name ("w")); (Int, Name ("a"))];
    body =
    StmtBlock ([Expression (CallMethod
                            (Super, [Identifier ("w"); Identifier ("a")]));
                Expression (Assign (Identifier ("cash"), Const (VInt (0))))])
    }
  ChildIntIntInt -> { args = [(Int, Name ("w")); (Int, Name ("a")); (Int, Name ("c"))];
    body =
    StmtBlock ([Expression (CallMethod
                            (Super, [Identifier ("w"); Identifier ("a")]));
                Expression (Assign (Identifier ("cash"), Identifier ("c")))])
    }
  ]]; children_keys : ; is_abstract : false; is_inheritable : true; parent_key : Person} 
  Person -> { this_key : Person; field_table : [[weight -> { f_type = Int; key = "weight"; is_mutable = false; f_value = None;
    sub_tree = None }
  age -> { f_type = Int; key = "age"; is_mutable = false; f_value = None;
    sub_tree = None }
  ]]; method_table : [[setWeightInt -> { m_type = Void; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = [(Int, Name ("w"))];
    key = "setWeightInt";
    body =
    Some (StmtBlock ([Expression (Assign
                                  (FieldAccess (This, Identifier ("weight")),
                                   Identifier ("w")))]))
    }
  getAge -> { m_type = Int; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = []; key = "getAge";
    body = Some (StmtBlock ([Return (Some (Identifier ("age")))])) }
  toString -> { m_type = String; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = []; key = "toString";
    body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))])) }
  equals -> { m_type = Int; is_abstract = false; is_overridable = true;
    has_override_annotation = false;
    args = [(ClassName ("Object"), Name ("obj"))]; key = "equals";
    body =
    Some (StmtBlock ([If
                      (Equal (This, Identifier ("obj")),
                       Return (Some (Const (VInt (1)))),
                       Some (Return (Some (Const (VInt (0))))))]))
    }
  setAgeInt -> { m_type = Void; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = [(Int, Name ("a"))];
    key = "setAgeInt";
    body =
    Some (StmtBlock ([Expression (Assign
                                  (FieldAccess (This, Identifier ("age")),
                                   Identifier ("a")))]))
    }
  getWeight -> { m_type = Int; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = []; key = "getWeight";
    body = Some (StmtBlock ([Return (Some (Identifier ("weight")))])) }
  ]]; constructor_table : [[PersonIntInt -> { args = [(Int, Name ("w")); (Int, Name ("a"))];
    body =
    StmtBlock ([Expression (Assign
                            (FieldAccess (This, Identifier ("weight")),
                             Identifier ("w")));
                Expression (Assign
                            (FieldAccess (This, Identifier ("age")),
                             Identifier ("a")))])
    }
  ]]; children_keys : Child ; is_abstract : false; is_inheritable : true; parent_key : Object} 
  Object -> { this_key : Object; field_table : [[]]; method_table : [[equals -> { m_type = Int; is_abstract = false; is_overridable = true;
    has_override_annotation = false;
    args = [(ClassName ("Object"), Name ("obj"))]; key = "equals";
    body =
    Some (StmtBlock ([If
                      (Equal (This, Identifier ("obj")),
                       Return (Some (Const (VInt (1)))),
                       Some (Return (Some (Const (VInt (0))))))]))
    }
  to_string -> { m_type = String; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = []; key = "toString";
    body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))])) }
  ]]; constructor_table : [[]]; children_keys : Person Main ; is_abstract : false; is_inheritable : true; parent_key : } 
  Main -> { this_key : Main; field_table : [[]]; method_table : [[toString -> { m_type = String; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = []; key = "toString";
    body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))])) }
  equals -> { m_type = Int; is_abstract = false; is_overridable = true;
    has_override_annotation = false;
    args = [(ClassName ("Object"), Name ("obj"))]; key = "equals";
    body =
    Some (StmtBlock ([If
                      (Equal (This, Identifier ("obj")),
                       Return (Some (Const (VInt (1)))),
                       Some (Return (Some (Const (VInt (0))))))]))
    }
  mainArray (String) -> { m_type = Void; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = [(Array (String), Name ("args"))];
    key = "mainArray (String)";
    body =
    Some (StmtBlock ([VarDec
                      (ClassName ("Person"),
                       [(Name ("p"),
                         Some (ClassCreate
                               (Name ("Person"),
                                [Const (VInt (80)); Const (VInt (45))])))]);
                      Expression (FieldAccess
                                  (FieldAccess
                                   (Identifier ("System"), Identifier ("out")),
                                   CallMethod
                                   (Identifier ("println"),
                                    [FieldAccess
                                     (Identifier ("p"),
                                      CallMethod (Identifier ("getWeight"), []))])));
                      VarDec
                      (ClassName ("Child"),
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
                                   (Identifier ("giveEvenNumbers100"), [])))]))
    }
  ]]; constructor_table : [[]]; children_keys : ; is_abstract : false; is_inheritable : true; parent_key : Object} 
  ]]
  -------------------SIMILAR_FIELDS-------------------
  
  Similar fields
  -------------------SIMILAR_METHODS_ERROR-------------------
  
  Method with this type exists
  -------------------SIMILAR_CONSTRUCTOR_ERROR-------------------
  
  Constructor with this type exists
  -------------------LACK_OF_SUPER_ERROR-------------------
  
  No super headed statement in inherited constructor
  -------------------ABSTRACTNESS_ERRORS-------------------
  
  Abstract method in non-abstract class
  No body of non-abstract method
  Abstract method cannot have body
  Abstract method must be overriden
  -------------------FINAL_MODIFIERS_ERRORS-------------------
  
  Final class cannot be inherited
  [[Child -> { this_key : Child; field_table : [[cash -> { f_type = Int; key = "cash"; is_mutable = false; f_value = None;
    sub_tree = None }
  weight -> { f_type = Int; key = "weight"; is_mutable = false; f_value = None;
    sub_tree = None }
  age -> { f_type = Int; key = "age"; is_mutable = false; f_value = None;
    sub_tree = None }
  ]]; method_table : [[toString -> { m_type = String; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = []; key = "toString";
    body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))])) }
  setCashInt -> { m_type = Void; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = [(Int, Name ("c"))];
    key = "setCashInt";
    body =
    Some (StmtBlock ([Expression (Assign
                                  (FieldAccess (This, Identifier ("cash")),
                                   Identifier ("c")))]))
    }
  getCash -> { m_type = Int; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = []; key = "getCash";
    body = Some (StmtBlock ([Return (Some (Identifier ("cash")))])) }
  equals -> { m_type = Int; is_abstract = false; is_overridable = true;
    has_override_annotation = false;
    args = [(ClassName ("Object"), Name ("obj"))]; key = "equals";
    body =
    Some (StmtBlock ([If
                      (Equal (This, Identifier ("obj")),
                       Return (Some (Const (VInt (1)))),
                       Some (Return (Some (Const (VInt (0))))))]))
    }
  getWeight -> { m_type = Int; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = []; key = "getWeight";
    body = Some (StmtBlock ([Return (Some (Identifier ("weight")))])) }
  ]]; constructor_table : [[ChildIntInt -> { args = [(Int, Name ("w")); (Int, Name ("a"))];
    body =
    StmtBlock ([Expression (CallMethod
                            (Super, [Identifier ("w"); Identifier ("a")]));
                Expression (Assign (Identifier ("cash"), Const (VInt (0))))])
    }
  ChildIntIntInt -> { args = [(Int, Name ("w")); (Int, Name ("a")); (Int, Name ("c"))];
    body =
    StmtBlock ([Expression (CallMethod
                            (Super, [Identifier ("w"); Identifier ("a")]));
                Expression (Assign (Identifier ("cash"), Identifier ("c")))])
    }
  ]]; children_keys : ; is_abstract : false; is_inheritable : true; parent_key : Person} 
  Person -> { this_key : Person; field_table : [[weight -> { f_type = Int; key = "weight"; is_mutable = false; f_value = None;
    sub_tree = None }
  age -> { f_type = Int; key = "age"; is_mutable = false; f_value = None;
    sub_tree = None }
  ]]; method_table : [[getAge -> { m_type = Int; is_abstract = false; is_overridable = false;
    has_override_annotation = false; args = []; key = "getAge";
    body = Some (StmtBlock ([Return (Some (Identifier ("age")))])) }
  toString -> { m_type = String; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = []; key = "toString";
    body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))])) }
  equals -> { m_type = Int; is_abstract = false; is_overridable = true;
    has_override_annotation = false;
    args = [(ClassName ("Object"), Name ("obj"))]; key = "equals";
    body =
    Some (StmtBlock ([If
                      (Equal (This, Identifier ("obj")),
                       Return (Some (Const (VInt (1)))),
                       Some (Return (Some (Const (VInt (0))))))]))
    }
  getWeight -> { m_type = Int; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = []; key = "getWeight";
    body = Some (StmtBlock ([Return (Some (Identifier ("weight")))])) }
  ]]; constructor_table : [[PersonIntInt -> { args = [(Int, Name ("w")); (Int, Name ("a"))];
    body =
    StmtBlock ([Expression (Assign
                            (FieldAccess (This, Identifier ("weight")),
                             Identifier ("w")));
                Expression (Assign
                            (FieldAccess (This, Identifier ("age")),
                             Identifier ("a")))])
    }
  ]]; children_keys : Child ; is_abstract : false; is_inheritable : true; parent_key : Object} 
  Object -> { this_key : Object; field_table : [[]]; method_table : [[equals -> { m_type = Int; is_abstract = false; is_overridable = true;
    has_override_annotation = false;
    args = [(ClassName ("Object"), Name ("obj"))]; key = "equals";
    body =
    Some (StmtBlock ([If
                      (Equal (This, Identifier ("obj")),
                       Return (Some (Const (VInt (1)))),
                       Some (Return (Some (Const (VInt (0))))))]))
    }
  to_string -> { m_type = String; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = []; key = "toString";
    body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))])) }
  ]]; constructor_table : [[]]; children_keys : Person Main ; is_abstract : false; is_inheritable : true; parent_key : } 
  Main -> { this_key : Main; field_table : [[]]; method_table : [[toString -> { m_type = String; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = []; key = "toString";
    body = Some (StmtBlock ([Return (Some (Const (VString ("Object"))))])) }
  equals -> { m_type = Int; is_abstract = false; is_overridable = true;
    has_override_annotation = false;
    args = [(ClassName ("Object"), Name ("obj"))]; key = "equals";
    body =
    Some (StmtBlock ([If
                      (Equal (This, Identifier ("obj")),
                       Return (Some (Const (VInt (1)))),
                       Some (Return (Some (Const (VInt (0))))))]))
    }
  mainArray (String) -> { m_type = Void; is_abstract = false; is_overridable = true;
    has_override_annotation = false; args = [(Array (String), Name ("args"))];
    key = "mainArray (String)";
    body =
    Some (StmtBlock ([VarDec
                      (ClassName ("Person"),
                       [(Name ("p"),
                         Some (ClassCreate
                               (Name ("Person"),
                                [Const (VInt (80)); Const (VInt (45))])))]);
                      Expression (FieldAccess
                                  (FieldAccess
                                   (Identifier ("System"), Identifier ("out")),
                                   CallMethod
                                   (Identifier ("println"),
                                    [FieldAccess
                                     (Identifier ("p"),
                                      CallMethod (Identifier ("getWeight"), []))])));
                      VarDec
                      (ClassName ("Child"),
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
                                   (Identifier ("giveEvenNumbers100"), [])))]))
    }
  ]]; constructor_table : [[]]; children_keys : ; is_abstract : false; is_inheritable : true; parent_key : Object} 
  ]]
  -------------------@OVERRIDE_ERRORS-------------------
  
  @Override annotation on not overriden method
