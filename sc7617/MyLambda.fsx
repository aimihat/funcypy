

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
    | Function of Identifier option * Identifier * Ast
    | FunctionDef of Identifier * Identifier * Ast

    // | Scope of Ast list option
    | Conditional of Ex * Ast * Ast option
    | FuncApp of Ast * Ast
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
    | BetaReductionT of LambdaType * LambdaType
    //| BetaReducion of LambdaType * LambdaType

and Env = (string*LambdaType) list    



//  Do i need them
let lambdaVar x = LambdaEx (Variable (IdString x))
let lambdaInt x = LambdaEx (Literal (Int x))
let lambdaDouble x = LambdaEx (Literal (Double x))
let lambdaString x = LambdaEx (Literal (String x))

let (|EXPVAR|_|) =
    let someExpVar x = Some(Variable (IdString x))
    let innerFn (inp: Ast) = 
        match inp with 
        | Expression (Variable (IdString inp)) -> Some inp
        | _ -> None
    innerFn


// Change AST to LambdaType
let (|REWRAP|_|) = 
    let innerFn inp = 
        match inp with
        | Expression ( Variable (IdString x)) -> Some (lambdaVar x)
        | Expression ( Literal (Int x)) -> Some (lambdaInt x)
        | Expression ( Literal (Double x)) -> Some (lambdaDouble x)
        | Expression ( Literal (String x)) -> Some (lambdaString x)
        | _ -> failwith "Failed to REWRAP"
    innerFn 

// Receive Function(None , indetifier "a" , AST) * Inp (Literal)

// focus interly on post-parsing Funcion to match it with LambdaType

let parse: (Ast -> LambdaType) = 
    let rec innerFn inp = 
        match inp with
        //  Evaluate Lambda
        | Function (None, IdString a, rest) -> LambdaT (a , (innerFn rest))
        
        // Beta reduction if second argument passed is a value or variable
        | FuncApp (Function (None, IdString a, rest), REWRAP b) -> BetaReductionT (LambdaT (a , (innerFn rest)) , b)
        // Evaluate Application
        | FuncApp (ast1, ast2) -> ApplicationT (innerFn ast1, innerFn ast2)
        //  Evaluate Variable
        | EXPVAR a -> lambdaVar a
        // Evaluate alpha reduction

        // evaluatio beta reduction
        | _ -> failwith "Lambda Parsing Error"
    innerFn

let eval: LambdaType -> LambdaType = 
    


// TEST simple parse

let inp1 = Function (None, IdString "a", FuncApp(Expression (Variable (IdString "a")), Expression (Variable (IdString "a"))))

print <| parse inp1