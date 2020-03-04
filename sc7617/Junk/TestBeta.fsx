


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
    | BetaReductionT of LambdaType * Ex
    //| BetaReducion of LambdaType * LambdaType

and Env = (string*LambdaType) list    


let (|MATCHVAR|_|) (arg: string) (newarg: string) = 
    let innerFn (inp: LambdaType) = 
        match inp with 
        | LambdaEx (Variable (IdString a)) when a.Equals(arg) -> Some (LambdaEx (Variable (IdString newarg)))
        | LambdaEx (Variable (IdString a)) -> Some (LambdaEx (Variable (IdString a))) 
        | _ -> failwith (sprintf "This match is not included, the given input is \n")
    innerFn

// recursively filter given Lambda Type
let rec filter body oldarg newarg = 
    match body with
    | LambdaT (arg, body) -> LambdaT(arg, filter body oldarg newarg)
    // Application should Have higer priority than MATCHVAR
    | ApplicationT (arg1, arg2) -> ApplicationT (filter arg1 oldarg newarg, filter arg2 oldarg newarg )
    | MATCHVAR oldarg newarg a -> a
    | _ -> failwith "Wront input to the filter function"



print <| filter (LambdaEx (Variable (IdString "a"))) "a" "b"
print <| filter (ApplicationT (LambdaEx (Variable (IdString "a")), LambdaEx (Variable (IdString "a")))) "a" "b"