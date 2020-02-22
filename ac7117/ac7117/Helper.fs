module Helper

type Arithmetic = Add | Subtract | Multiply | Divide
type Comparison = Eq | Ne | Lt | Gt | Le | Ge
type Identifier = string

type Value =
    //| Bool of bool
    | Int of int
    //| Double of double
    //| String of string
    //| Tuple of Value * Value
    //| List of Value * Value

type Ast =    
    //| Statement of Ast
    | Expression of Ex
    | Function of Identifier option * Identifier * Ast
    //| Scope of Ast list option
    //| Conditional of Ex * Ast * Ast option
    | Call of Ast * Ast
    //| Assign of Identifier * Ex
    | Combinator of CombinatorType
and Ex =
    | Single of Ast
    | Literal of Value
    | Variable of Identifier
    | Arithmetic of Ex * Arithmetic * Ex
    //| Comparison of Ex * Comparison * Ex
and CombinatorType = | K | I | S