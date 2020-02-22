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



// Suppose I am give the input as "LambdaT "a" 'a'"" which is equivalent to //a.a where // means lambda

let rec evalInEnv (env: Env) (term: LambdaTerm): LambdaTerm =
  match term with
    | EXPT name ->
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
    | EXPT name -> [name]
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

let inp1 = LambdaT ('a', (ExT (Variable (IdChar 'a'))))

print <| interpString inp1







