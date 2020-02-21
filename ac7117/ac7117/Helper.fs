module Helper


// Incomplete type definition, closures implemention TBD
type Arithmetic = Add | Subtract | Multiply | Divide
type Comparison = Eq | Ne | Lt | Gt | Le | Ge
type Identifier = string


type Value =
    | Bool of bool
    | Int of int
    | Double of double
    | String of string
    | Tuple of Value*Value
    | List of Value*Value


type Ast =    
    | Statement of Ast    
    | Expression of Ex    
    | Function of string option * Argument list option * Ast
    | Scope of Ast list option
    | Conditional of Ex * Ast * Ast option
    | Call of Identifier * Argument list option
    | Assign of Identifier * Ex
and Ex =
    | Single of Ast
    | Literal of Value
    | Variable of Identifier
    | Arithmetic of Ex * Arithmetic * Ex
    | Comparison of Ex * Comparison * Ex
and Argument =
    | Element of Ex