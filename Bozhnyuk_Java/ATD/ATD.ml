module ATD = struct

  type jModifier = 
    | Public
    | Static
    | Final
    | Abstract [@@deriving show]

  type jType =
    | JInt
    | JBoolean
    | JVoid
    | JRef of string
    | JChar
    | JString
    | JArray of jType
    | JObject [@@deriving show]


  type jValue = 
    | JVBool of bool
    | JVInt of int
    | JVNull
    | JVChar of char
    | JVArray of jValue list
    | JVVoid
    | JString
    | JVRef
    | JVObject [@@deriving show]

  type jException = {jName: jType ; message : string} [@@deriving show]

  type jVariable = {vModifiers : jModifier list; isMutable : bool; varType : jType; varName : string; value : jValue option} [@@deriving show]

  type jNumericExpr = 
    | Add of jExpr * jExpr
    | Sub of jExpr * jExpr
    | Mult of jExpr * jExpr
    | Div of jExpr * jExpr
    | Mod of jExpr * jExpr
    | PrefAdd of jExpr
    | PrefSub of jExpr
    | PostAdd of jExpr
    | PostSub of jExpr [@@deriving show]

  and jLogicalExpr = 
    | And of jExpr * jExpr
    | Or of jExpr * jExpr
    | Not of jExpr [@@deriving show]

  and jTestExpr = 
    | Equal of jExpr * jExpr
    | NotEqual of jExpr * jExpr
    | Less of jExpr * jExpr
    | More of jExpr * jExpr
    | LessOrEqual of jExpr * jExpr
    | MoreOrEqual of jExpr * jExpr [@@deriving show]

  and jCreatingExpr = 
    | ClassDec of {clName : string; argList : jExpr list} (*new clName(argList)*)
    | ArrayDec of {arrType : jType; cntExpr : jExpr}    (*new arrType[cntExpr]*) [@@deriving show]


  and jExpr =
    | NumericExpr of jNumericExpr
    | LogicalExpr of jLogicalExpr
    | TestingExpr of jTestExpr
    | CreatingExpr of jCreatingExpr
    | CallMethod of {name : string; args : jExpr list}
    | Identifier of string 
    | Const of jValue
    | This
    | Super
    | Null
    | Access of jExpr * jExpr
    | ArrayAccess of jExpr * jExpr
    | Many of jExpr * jExpr [@@deriving show]


  and jStat =
    | If of {cond : jExpr; thenStat : jStat list; elseStat : jStat list}
    | While of {cond : jExpr; body : jStat list}
    | For of {varDec : jStat; cond : jExpr; afterBody : jStat list ; body : jStat list}
    | Break
    | Continue
    | Return of {result : jExpr}
    | VarDec of {name : string; vType : jType; value : jValue option}  
    | Expression of jExpr 
    | Throw of {exc : jException} [@@deriving show]



  type jMethod = {mModifiers: jModifier list; mName : string; mRetType : jType; mArgs : jExpr list; mBody : jStat list;}  [@@deriving show]

  type jClass = {cModifiers: jModifier list; cName : string; cFields : jVariable list; cMethods : jMethod list; cClasses : jClass list; cParent : jClass option} [@@deriving show]

end


  
