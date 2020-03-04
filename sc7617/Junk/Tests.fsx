/// Helpful functions

let print x = printf "%A \n" x
let printPipe x = printf "%A \n" x ; x





// Incomplete type definition, closures implemention TBD




type Arithmetic = Add | Subtract | Multiply | Divide
type Comparison = Eq | Ne | Lt | Gt | Le | Ge
type Identifier = 
    | IdString of string 
    | IdChar of char/// shall we replace this just by string???
 
type Value =
    | Bool of bool
    | Int of int
    | Double of double
    | String of string
    | Char of char
    | Tuple of Value*Value
    //| List of Value*Tuple
 
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

and LambdaTerm =
  | ExT of Ex
  | LambdaT of char*LambdaTerm
  | ClosureT of char*LambdaTerm*Env
  | ApplicationT of LambdaTerm*LambdaTerm

and Env = (char*LambdaTerm) list    


let (|EXPT|_|) = 
    let innerFn inp = 
        match inp with
        | ExT (Variable (IdChar name)) -> Some name
        | _ -> failwith "Failed to match"

    innerFn

let expt = (|EXPT|_|)


print <| expt (ExT (Variable (IdChar 'a')))