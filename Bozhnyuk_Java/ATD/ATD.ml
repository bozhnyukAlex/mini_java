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

    


  type jExpr =
    | Add of jExpr * jExpr
    | Sub of jExpr * jExpr
    | Mult of jExpr * jExpr
    | Div of jExpr * jExpr
    | Mod of jExpr * jExpr
    | PrefAdd of jExpr
    | PrefSub of jExpr
    | PostAdd of jExpr
    | PostSub of jExpr 
    | And of jExpr * jExpr
    | Or of jExpr * jExpr
    | Not of jExpr 
    | Equal of jExpr * jExpr
    | NotEqual of jExpr * jExpr
    | Less of jExpr * jExpr
    | More of jExpr * jExpr
    | LessOrEqual of jExpr * jExpr
    | MoreOrEqual of jExpr * jExpr 
    | ClassCreate of string * jExpr list (*new clName(argList)*)
    | ArrayCreate of jType * jExpr list (*new arrType[cntExpr]*) 
    | CallMethod of jExpr * jExpr list  
    | Identifier of string 
    | Const of jValue
    | This
    | Super
    | Null
    | FieldAccess of jExpr * jExpr
    | ArrayAccess of jExpr * jExpr list
    | Assign of jExpr * jExpr
    
  and jStat =
    | If of jExpr * jStat * jStat option (*cond * thenStat * elseStat*)
    | While of jExpr * jStat (* cond * body *)
    | For of jStat option * jExpr option * jExpr list * jStat(* varDec * jExpr * afterBody * body *)
    | Break
    | Continue
    | Return of jExpr option (* result *)
    | StatBlock of jStat list
    | VarDec of jModifier list * jType * ((jExpr * (jExpr option)) list)
    | Expression of jExpr 
    | Throw of jExpr


  type jMethod = {mModifiers: jModifier list; mName : string; mRetType : jType; mArgs : jExpr list; mBody : jStat list;}  

  type jClass = {cModifiers: jModifier list; cName : string; cFields : jVariable list; cMethods : jMethod list; cClasses : jClass list; cParent : jClass option}  

end


  
