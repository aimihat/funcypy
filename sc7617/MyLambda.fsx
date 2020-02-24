

/// Helpful functions

let print x = printf "%A \n" x
let printPipe x = printf "%A \n" x ; x





// Incomplete type definition, closures implemention TBD




type Arithmetic = Add | Subtract | Multiply | Divide
type Comparison = Eq | Ne | Lt | Gt | Le | Ge
type Identifier = IdString of string


type Value =
    | Bool of bool
    | Int of int
    | Double of double
    | String of string
    | Tuple of Value*Value

type Ast =    
    | Statement of Ast    
    | Expression of Ex    
    | Function of Identifier option * Identifier * Ast option 
    | FunctionDef of Identifier * Identifier * Ast

    // | Scope of Ast list option
    | Conditional of Ex * Ast * Ast option
    | Call of Ast * Ast
    // | Assign of Identifier * Ex
    | Combinator of CombinatorType
and Ex =
    | Single of Ast
    | Literal of Value
    | Variable of Identifier
    | Arithmetic of Ex * Arithmetic * Ex
    | Comparison of Ex * Comparison * Ex
and CombinatorType = 
    | K 
    | I 
    | S

and LambdaType = 
//  Funcion (None, Identifier (String x), Ast) -> LambdaT (Identifier, Ast)
    | LambdaEx of Ex
    | LambdaT of string * LambdaType
    | ClosureT of string  * LambdaType*Env
    | ApplicationT of LambdaType * LambdaType
    //| BetaReducion of LambdaType * LambdaType

and Env = (string*LambdaType) list    



//  Do i need them
let lambdaVar x = LambdaEx (Variable (IdString x))
let lambdaInt x = LambdaEx (Literal (Int x))
let lambdaDouble x = LambdaEx (Literal (Double x))
let lambdaString x = LambdaEx (Literal (String x))





// Receive Function(None , indetifier "a" , AST) * Inp (Literal)

// focus interly on post-parsing Funcion to match it with LambdaType

let rec parse: (Ast -> LambdaType) = 
    let innerFn inp = 
        match inp with
        //  
        | Function (None, IdString a, Some rest) -> LambdaT (a , (parse rest))
        //  evaluate application case
        // if there is something left
        | Function (Some (IdString a), IdString b, Some rest) -> ApplicationT (ApplicationT (lambdaVar a, lambdaVar b), parse rest)
        //  if there is none left
        | Function (Some (IdString a), IdString b, None) -> ApplicationT (lambdaVar a, lambdaVar b)
        //  Evaluate Variable
        | Expression (Variable (IdString a)) -> lambdaVar a
        // Evaluate alpha reduction

        // evaluatio beta reduction
        | _ -> failwith "Lambda Parsing Error"
    innerFn