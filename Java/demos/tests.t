  $ (cd ../../../../default && demos/demoFirst.exe)
  Atd.Class
  ([Atd.Public], Atd.Identifier ("Main"), None,
   [Atd.Method
    ([Atd.Public; Atd.Static], Atd.JVoid, Atd.Identifier ("main"),
     [(Atd.JArray (Atd.JString), Atd.Identifier ("args"))],
     Some (Atd.StatBlock ([Atd.VarDec
                           (Atd.JClassName ("Person"),
                            [(Atd.Identifier ("p"),
                              Some (Atd.ClassCreate
                                    ("Person",
                                     [Atd.Const (Atd.JVInt (80));
                                      Atd.Const (Atd.JVInt (45))])))]);
                           Atd.Expression (Atd.FieldAccess
                                           (Atd.FieldAccess
                                            (Atd.Identifier ("System"),
                                             Atd.Identifier ("out")),
                                            Atd.CallMethod
                                            (Atd.Identifier ("println"),
                                             [Atd.FieldAccess
                                              (Atd.Identifier ("p"),
                                               Atd.CallMethod
                                               (Atd.Identifier ("getWeight"),
                                                []))])));
                           Atd.VarDec
                           (Atd.JClassName ("Child"),
                            [(Atd.Identifier ("ch"),
                              Some (Atd.ClassCreate
                                    ("Child",
                                     [Atd.Const (Atd.JVInt (66));
                                      Atd.Const (Atd.JVInt (20))])))]);
                           Atd.Expression (Atd.FieldAccess
                                           (Atd.Identifier ("ch"),
                                            Atd.CallMethod
                                            (Atd.Identifier ("setCash"),
                                             [Atd.Const (Atd.JVInt (50))])));
                           Atd.Expression (Atd.FieldAccess
                                           (Atd.Identifier ("ch"),
                                            Atd.CallMethod
                                            (Atd.Identifier ("giveEvenNumbers100"),
                                             [])))])))])
  Atd.Class
  ([], Atd.Identifier ("Person"), None,
   [Atd.VarField ([Atd.Public], Atd.JInt, [(Atd.Identifier ("weight"), None)]);
    Atd.VarField ([Atd.Public], Atd.JInt, [(Atd.Identifier ("age"), None)]);
    Atd.Constructor
    ([Atd.Public], Atd.Identifier ("Person"),
     [(Atd.JInt, Atd.Identifier ("w")); (Atd.JInt, Atd.Identifier ("a"))],
     Atd.StatBlock ([Atd.Expression (Atd.Assign
                                     (Atd.FieldAccess
                                      (Atd.This, Atd.Identifier ("weight")),
                                      Atd.Identifier ("w")));
                     Atd.Expression (Atd.Assign
                                     (Atd.FieldAccess
                                      (Atd.This, Atd.Identifier ("age")),
                                      Atd.Identifier ("a")))]));
    Atd.Method
    ([Atd.Public], Atd.JInt, Atd.Identifier ("getWeight"), [],
     Some (Atd.StatBlock ([Atd.Return (Some (Atd.Identifier ("weight")))])));
    Atd.Method
    ([Atd.Public], Atd.JInt, Atd.Identifier ("getAge"), [],
     Some (Atd.StatBlock ([Atd.Return (Some (Atd.Identifier ("age")))])));
    Atd.Method
    ([Atd.Public], Atd.JVoid, Atd.Identifier ("setWeight"),
     [(Atd.JInt, Atd.Identifier ("w"))],
     Some (Atd.StatBlock ([Atd.Expression (Atd.Assign
                                           (Atd.FieldAccess
                                            (Atd.This,
                                             Atd.Identifier ("weight")),
                                            Atd.Identifier ("w")))])));
    Atd.Method
    ([Atd.Public], Atd.JVoid, Atd.Identifier ("setAge"),
     [(Atd.JInt, Atd.Identifier ("a"))],
     Some (Atd.StatBlock ([Atd.Expression (Atd.Assign
                                           (Atd.FieldAccess
                                            (Atd.This, Atd.Identifier ("age")),
                                            Atd.Identifier ("a")))])))])
  Atd.Class
  ([], Atd.Identifier ("Child"), Some (Atd.Identifier ("Person")),
   [Atd.VarField ([Atd.Public], Atd.JInt, [(Atd.Identifier ("cash"), None)]);
    Atd.Constructor
    ([Atd.Public], Atd.Identifier ("Child"),
     [(Atd.JInt, Atd.Identifier ("w")); (Atd.JInt, Atd.Identifier ("a"))],
     Atd.StatBlock ([Atd.Expression (Atd.CallMethod
                                     (Atd.Super,
                                      [Atd.Identifier ("w");
                                       Atd.Identifier ("a")]));
                     Atd.Expression (Atd.Assign
                                     (Atd.Identifier ("cash"),
                                      Atd.Const (Atd.JVInt (0))))]));
    Atd.Method
    ([Atd.Public], Atd.JInt, Atd.Identifier ("getCash"), [],
     Some (Atd.StatBlock ([Atd.Return (Some (Atd.Identifier ("cash")))])));
    Atd.Method
    ([Atd.Public], Atd.JVoid, Atd.Identifier ("setCash"),
     [(Atd.JInt, Atd.Identifier ("c"))],
     Some (Atd.StatBlock ([Atd.Expression (Atd.Assign
                                           (Atd.FieldAccess
                                            (Atd.This, Atd.Identifier ("cash")),
                                            Atd.Identifier ("c")))])));
    Atd.Constructor
    ([Atd.Public], Atd.Identifier ("Child"),
     [(Atd.JInt, Atd.Identifier ("w")); (Atd.JInt, Atd.Identifier ("a"));
      (Atd.JInt, Atd.Identifier ("c"))],
     Atd.StatBlock ([Atd.Expression (Atd.CallMethod
                                     (Atd.Super,
                                      [Atd.Identifier ("w");
                                       Atd.Identifier ("a")]));
                     Atd.Expression (Atd.Assign
                                     (Atd.Identifier ("cash"),
                                      Atd.Identifier ("c")))]));
    Atd.Method
    ([Atd.Public], Atd.JVoid, Atd.Identifier ("giveEvenNumbers100"), [],
     Some (Atd.StatBlock ([Atd.For
                           (Some (Atd.VarDec
                                  (Atd.JInt,
                                   [(Atd.Identifier ("i"),
                                     Some (Atd.Const (Atd.JVInt (0))))])),
                            Some (Atd.Less
                                  (Atd.Identifier ("i"),
                                   Atd.Const (Atd.JVInt (100)))),
                            [Atd.PostInc (Atd.Identifier ("i"))],
                            Atd.StatBlock ([Atd.If
                                            (Atd.And
                                             (Atd.Equal
                                              (Atd.Mod
                                               (Atd.Identifier ("i"),
                                                Atd.Const (Atd.JVInt (2))),
                                               Atd.Const (Atd.JVInt (0))),
                                              Atd.Not (Atd.Equal
                                                       (Atd.Mod
                                                        (Atd.Identifier ("i"),
                                                         Atd.Const (Atd.JVInt (2))),
                                                        Atd.Const (Atd.JVInt (1))))),
                                             Atd.StatBlock ([Atd.Expression (
                                                              Atd.FieldAccess
                                                              (Atd.FieldAccess
                                                               (Atd.Identifier ("System"),
                                                                Atd.Identifier ("out")),
                                                               Atd.CallMethod
                                                               (Atd.Identifier ("println"),
                                                                [Atd.Identifier ("i")])))]),
                                             Some (Atd.StatBlock ([Atd.Continue])))]))])))])

