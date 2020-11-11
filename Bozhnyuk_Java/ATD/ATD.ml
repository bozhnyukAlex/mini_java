module ATD = struct

  type jModifier = 
    | Public
    | Static
    | Final
    | Abstract

  type jType =
    | JInt
    | JVoid
    | JRef of string
    | JString
    | JArray of jType
    | JObject 


  type jValue = 
    | JVBool of bool
    | JVInt of int
    | JVNull
    | JVChar of char
    | JVArray of jValue list
    | JVVoid
    | JVString of string
    | JVRef
    | JVObject

  type jException = {jName: jType ; message : string} 

  type jVariable = {vModifiers : jModifier list; isMutable : bool; varType : jType; varName : string; value : jValue option} 

  type jNumericExpr = 
    | Add of jExpr * jExpr
    | Sub of jExpr * jExpr
    | Mult of jExpr * jExpr
    | Div of jExpr * jExpr
    | Mod of jExpr * jExpr
    | PrefAdd of jExpr
    | PrefSub of jExpr
    | PostAdd of jExpr
    | PostSub of jExpr 

  and jLogicalExpr = 
    | And of jExpr * jExpr
    | Or of jExpr * jExpr
    | Not of jExpr 

  and jTestExpr = 
    | Equal of jExpr * jExpr
    | NotEqual of jExpr * jExpr
    | Less of jExpr * jExpr
    | More of jExpr * jExpr
    | LessOrEqual of jExpr * jExpr
    | MoreOrEqual of jExpr * jExpr 

  and jCreatingExpr = 
    | ClassCreate of string * jExpr list (*new clName(argList)*)
    | ArrayCreate of jType * jExpr list (*new arrType[cntExpr]*) 


  and jExpr =
    | NumericExpr of jNumericExpr
    | LogicalExpr of jLogicalExpr
    | TestingExpr of jTestExpr
    | CreatingExpr of jCreatingExpr
    | CallMethod of jExpr * jExpr list  
    | Identifier of string 
    | Const of jValue
    | This
    | Super
    | Null
    | FieldAccess of jExpr * jExpr
    | ArrayAccess of jExpr * jExpr list


  

    
  and jStat =
    | If of {cond : jExpr; thenStat : jStat list; elseStat : jStat list}
    | While of {cond : jExpr; body : jStat list}
    | For of {varDec : jStat; cond : jExpr; afterBody : jStat list ; body : jStat list}
    | Break
    | Continue
    | Return of {result : jExpr}
    | VarDec of {name : string; vType : jType; value : jValue option}  
    | Expression of jExpr 
    | Throw of {exc : jException} 


  type jMethod = {mModifiers: jModifier list; mName : string; mRetType : jType; mArgs : jExpr list; mBody : jStat list;}  

  type jClass = {cModifiers: jModifier list; cName : string; cFields : jVariable list; cMethods : jMethod list; cClasses : jClass list; cParent : jClass option}  

end


  
