
/// Helpful functions

let print x = printf "%A \n" x
let printPipe x = printf "%A \n" x ; x





// Incomplete type definition, closures implemention TBD




type Arithmetic = Add | Subtract | Multiply | Divide
type Comparison = Eq | Ne | Lt | Gt | Le | Ge
type Identifier = 
    | IdString of string
    | IdChar of char

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

// and LambdaType = 
// //  Funcion (None, Identifier (String x), Ast) -> LambdaT (Identifier, Ast)
//     | LambdaEx of Ex
//     | LambdaT of Identifier * Ast
//     | ClosureT of Identifier * LambdaType
//     | ApplicationT of LambdaType * LambdaType


    

type LambdaTerm =
  | VariableT of string
  | LambdaT of string*LambdaTerm
  | ClosureT of string*LambdaTerm*Env
  | ApplicationT of LambdaTerm*LambdaTerm

and Env = (char*LambdaTerm) list    






let rec evalInEnv (env: Env) (term: LambdaTerm): LambdaTerm =
  match term with
    | VariableT name ->
      match List.tryFind (fun (aName, term) -> aName = name) env with
        | Some (_, term) -> term
        | None -> failwith "Couldn't find a term by name"
    | LambdaT (arg, body) ->
      ClosureT (arg, body, env)
    | ApplicationT (fn, value) ->
      match evalInEnv env fn with
        | ClosureT (arg, body, closedEnv) ->
          let evaluatedValue = evalInEnv env value

          let newEnv = (arg, evaluatedValue)::closedEnv @ env

          evalInEnv newEnv body
        | _ ->
          failwith "Cannot apply something given"
    | closure -> closure


let eval (term: LambdaTerm): LambdaTerm =
  evalInEnv [] term















let rec pretty (term: LambdaTerm): char list =
  match term with
    | VariableT name -> [name]
    | LambdaT (arg, body) -> ['\\'; arg; '.'] @ pretty body
    | ClosureT (arg, body, _) -> ['\\'; arg; '.'] @ pretty body
    | ApplicationT (fn, value) -> ['('] @ pretty fn @ [' '] @ pretty value @ [')']

let interp: LambdaTerm -> char list =

    eval
    >> pretty

let interpString: LambdaTerm -> string =
    interp
    >> List.map string
    >> String.concat ""




/// Test Inputs
/// 

// Input 1

let inp1 = ApplicationT (LambdaT ('a', VariableT 'a'), LambdaT ('c', ApplicationT (VariableT 'c' , VariableT 'c')))

print <| interpString inp1







