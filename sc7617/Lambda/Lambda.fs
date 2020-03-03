module Lambda

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
    | Function of Identifier option * Identifier * Ast // Lambda (arg, body)
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
    // replace by beta reduction
    | ApplicationT of LambdaType * LambdaType
    | BetaReductionT of LambdaType * LambdaType
    //| BetaReducion of LambdaType * LambdaType

//and Env = (string*LambdaType) list    
and Env =  Map<string, LambdaType>








//////////////////////////////////////////////////////////////////
//////////////////////////                  //////////////////////
//////////////////////////   PARSING        //////////////////////
//////////////////////////                  //////////////////////
//////////////////////////////////////////////////////////////////


let (|LAMBDAMATCH|_|) = 
    let innerFn inp = 
        match inp with 
        | Function (None, IdString arg, body) -> Some (arg, body)
        | _ -> None // used to be failwith
    innerFn

// Receive Function(None , indetifier "a" , AST) * Inp (Literal)

// focus interly on post-parsing Funcion to match it with LambdaType


let parse: (Result<Ast,string> -> Result<LambdaType,string>) = 
    let validate f inp = 
        match f inp with
        | Error msg -> failwith msg
        | Ok exp -> Some exp
                    
    let rec innerFn inp = 
        match inp with
        // If error recieved from parsing
        | Error msg -> Error msg
        //  Function (Lambda)
        | Ok (LAMBDAMATCH (id, body)) -> 
            match innerFn (Ok body) with
            | Error msg -> Error msg
            | Ok exp ->  Ok (LambdaT (id , exp))
            // Why does the rule below will never be mathced?
            //| _ -> Error "Invalid type of Lambda"

        // all expressions
        | Ok (Expression (exp)) -> Ok (LambdaEx (exp))

        // Invalid function application
        | Ok (FuncApp (Expression _, LAMBDAMATCH _)) -> Error "not acceptable match (variable lambda)"
        // Match conditionals and comparators
        | Ok (FuncApp (LAMBDAMATCH (id, body), exp)) -> 
            match innerFn (Ok body), innerFn (Ok exp) with
            | Error msg, Error _ -> Error msg
            | Ok body, Ok exp -> Ok (BetaReductionT (LambdaT (id, (body)), (exp)))
            | _ -> Error "Invalid Beta Reduction"
        | Ok (FuncApp (func, exp)) -> 
            match innerFn (Ok func), innerFn (Ok exp) with
            | Error msg, Error _ -> Error msg
            | Ok func, Ok exp -> Ok (ApplicationT (func , exp))
            | _ -> Error "Invalid Application Parsing Match"
     
        | _ -> Error "Lambda Parsing Error"
    innerFn


//////////////////////////////////////////////////////////////////
//////////////////////////                  //////////////////////
//////////////////////////   ARITHMETICS    //////////////////////
//////////////////////////                  //////////////////////
//////////////////////////////////////////////////////////////////


let (|LAMBDAVAR|_|) =
    let innerFn inp = 
        match inp with
        | LambdaEx (Variable (IdString a)) -> Some a
        | _ -> None
    innerFn

let (|MAKEDOUBLE|_|) = 
    let innerFn (inp: Ex): Ex option = 
        match inp with
        | Literal (Int a) -> Some (Literal (Double (float a)))
        | Literal (Double a) -> Some (Literal (Double a))
        | _ -> 
            let err = sprintf "Failed to get double, %A is given" inp
            failwith err
    innerFn

let (|ADD|_|) = 
    let someLit x = Some (Literal x) 
    let innerFn ((n1:Ex), (n2: Ex)): Ex option = 
        match (n1, n2) with
        | (Literal (String a), Literal(String b)) -> someLit (String (a + b))
        | (Literal (Int a), Literal (Int b)) -> someLit (Int(a + b))
        | (MAKEDOUBLE (Literal (Double a)), MAKEDOUBLE (Literal (Double b))) -> someLit (Double (a + b))
        | _ -> 
            let error = sprintf "Addition Failed, %A is given " (n1, n2)
            failwith error
    innerFn

let (|SUB|_|) = 
    let someLit x = Some (Literal x) 
    let innerFn ((n1:Ex), (n2: Ex)): Ex option = 
        match (n1, n2) with
        | (Literal (Int a), Literal (Int b)) -> someLit (Int(a - b))
        | (MAKEDOUBLE (Literal (Double a)), MAKEDOUBLE (Literal (Double b))) -> someLit (Double (a - b))
        | _ -> 
            let error = sprintf "Subtraction Failed, %A is given " (n1, n2)
            failwith error
    innerFn

let (|MULT|_|) = 
    let someLit x = Some (Literal x) 
    let innerFn ((n1:Ex), (n2: Ex)): Ex option = 
        match (n1, n2) with
        | (Literal (Int a), Literal (Int b)) -> someLit (Int(a * b))
        | (MAKEDOUBLE (Literal (Double a)), MAKEDOUBLE (Literal (Double b))) -> someLit (Double (a * b))
        | _ -> 
            let error = sprintf "Multiplication Failed, %A is given " (n1, n2)
            failwith error
    innerFn

let (|DIVIDE|_|) = 
    let someLit x = Some (Literal x) 
    let innerFn ((n1:Ex), (n2: Ex)): Ex option = 
        match (n1, n2) with
        | (Literal (Int a), Literal (Int b)) -> someLit (Int(a / b))
        | (MAKEDOUBLE (Literal (Double a)), MAKEDOUBLE (Literal (Double b))) -> someLit (Double (a / b))
        | _ -> 
            let error = sprintf "Division Failed, %A is given " (n1, n2)
            failwith error
    innerFn

let lambdaVar x = LambdaEx (Variable (IdString x))
let ExVar x = Variable (IdString x)






let (|EVALARITHM|_|) = 
    // Overload == operator locally to match arithmetic signs
    let (==) (sign1: Arithmetic) (sign2: Arithmetic) =
        match sign1, sign2 with
        | Add, Add -> true
        | Subtract, Subtract -> true
        | Multiply, Multiply -> true
        | Divide, Divide -> true
        | _ -> false
    let innerFn (inp: Ex): Ex option = 
        match inp with
        | Arithmetic (exp1, sign, exp2) ->
            match (exp1,  exp2) with
            | (Variable (IdString a), Variable(IdString b)) -> Some (Arithmetic (ExVar a, sign, ExVar b))
            | ADD result when sign == Add -> Some (result)
            | SUB result when sign == Subtract -> Some (result)
            | MULT result when sign == Multiply -> Some (result)
            | DIVIDE result when sign == Divide -> Some (result)           
            | _ -> failwith "sign is not recognised"
        | _ -> None
    innerFn






/////////////////    Comparison Evaluation     ///////////////////

// type Comparison = Eq | Ne | Lt | Gt | Le | Ge


let (|EQUAL|_|) = 
    let someLit x = Some (Literal x) 
    let innerFn ((n1:Ex), (n2: Ex)): Ex option = 
        match (n1, n2) with
        | (Literal (String a), Literal (String b)) -> someLit (Bool (a = b))
        | (Literal (Int a), Literal (Int b)) -> someLit (Bool(a = b))
        | (MAKEDOUBLE (Literal (Double a)), MAKEDOUBLE (Literal (Double b))) -> someLit (Bool (a = b))
        | (Literal (Bool a), Literal (Bool b)) -> someLit (Bool (a = b))
        | _ -> 
            let error = sprintf "Equal Test Failed, %A was given " (n1, n2)
            failwith error
    innerFn

let (|NEQUAL|_|) = 
    let someLit x = Some (Literal x) 
    let innerFn inp = 
        match inp with
        | EQUAL (Literal (Bool result)) -> someLit (Bool (not result))
        | _ -> 
            let error = sprintf "Equal Test Failed, %A was given " inp
            failwith error
    innerFn

let (|LTHAN|_|) = 
    let someLit x = Some (Literal x) 
    let innerFn ((n1:Ex), (n2: Ex)): Ex option = 
        match (n1, n2) with
        | (Literal (Int a), Literal (Int b)) -> someLit (Bool(a < b))
        | (MAKEDOUBLE (Literal (Double a)), MAKEDOUBLE (Literal (Double b))) -> someLit (Bool (a < b))
        | _ -> 
        let msg = sprintf "Less Conditional Failed, %A was given" (n1, n2)
        failwith msg
    innerFn

let (|GTHAN|_|) = 
    let someLit x = Some (Literal x) 
    let innerFn ((n1:Ex), (n2: Ex)): Ex option = 
        match (n1, n2) with
        | (Literal (Int a), Literal (Int b)) -> someLit (Bool(a > b))
        | (MAKEDOUBLE (Literal (Double a)), MAKEDOUBLE (Literal (Double b))) -> someLit (Bool (a > b))
        | _ -> 
        let msg = sprintf "Greater Comparison Failed, %A was given" (n1, n2)
        failwith msg
    innerFn

let (|LEQTHAN|_|) = 
    let innerFn (inp: Ex*Ex): Ex option = 
        match inp with
        | LTHAN result -> Some result
        | EQUAL result -> Some result
        | _ -> 
        let msg = sprintf "Less Equal Comparison Failed, %A was given" inp
        failwith msg
    innerFn

let (|GEQTHAN|_|) = 
    let innerFn (inp: Ex*Ex): Ex option = 
        match inp with
        | GTHAN result -> Some result
        | EQUAL result -> Some result
        | _ -> 
        let msg = sprintf "Less Equal Comparison Failed, %A was given" inp
        failwith msg
    innerFn

let (|EVALCOMP|_|) =
    // Overload operator locally
    let (==) (sign1: Comparison) (sign2: Comparison) =
        match sign1, sign2 with
        | Eq, Eq -> true
        | Ne, Ne -> true
        | Lt, Lt -> true
        | Gt, Gt -> true
        | Le, Le -> true
        | Ge, Ge -> true
        | _ -> false
    let innerFn (inp: Ex): Ex option = 
        match inp with
        | Comparison (exp1, sign , exp2) ->
            match (exp1,  exp2) with
            | (Variable (IdString a), Variable(IdString b)) -> Some (Comparison (ExVar a, sign, ExVar b))
            | EQUAL result when sign == Eq -> Some (result)
            | NEQUAL result when sign == Ne -> Some (result)
            | LTHAN result when sign == Lt -> Some (result)           
            | GTHAN result when sign == Gt -> Some (result)
            | LEQTHAN result when sign == Le -> Some (result)
            | GEQTHAN result when sign == Ge -> Some (result)
            | _ -> failwith "sign is not recognised"
        | _ -> None
    innerFn

//////////////       END COMPARISON EVALUATION          ///////////////////








//////////////////////////////////////////////////////////////////
//////////////////////////                  //////////////////////
//////////////////////////   EVALUATION     //////////////////////
//////////////////////////                  //////////////////////
//////////////////////////////////////////////////////////////////

let rec eval (env: Env) (inp: Result<LambdaType,string>): Result<LambdaType,string> =
    let innerFn =  
        match inp with 
        // make sure this variable has been declared before and therefore perform search through the environment 
        | Ok (LambdaEx (Variable (IdString exp))) -> 
            match Map.tryFind exp env with
                | Some term -> Ok term
                | None -> Error "Couldn't find a Variable in the environment"

        | Ok (LambdaEx (EVALARITHM result)) -> 
            /// result itself is an expression. It can give us either Literal or Variable
            /// so if it is literal we return literal, if this is expression of variable we perform search for each variable
            match result with
            | Arithmetic (Variable (IdString a), sign, Variable (IdString b)) ->
                let evaluatedA =  eval env (Ok (lambdaVar a))
                let evaluatedB =  eval env (Ok (lambdaVar b))
                match evaluatedA , evaluatedB with
                | Ok (LambdaEx (Literal (a))), Ok (LambdaEx (Literal b)) -> 
                    match Arithmetic (Literal a, sign, Literal b) with
                    | EVALARITHM result -> Ok (LambdaEx result)
                    | _ -> failwith "failed to calculate arithmetic"
                | Ok (LambdaEx a), Ok (LambdaEx b) -> Ok (LambdaEx (Arithmetic (a, sign, b)))
                | _ -> failwith "Failed to evaluate Arithmetic from the environment"
                //| _ -> failwith "Cannot Evaluate"
            
            | Literal (x) -> Ok (LambdaEx (Literal x))
            | _ -> failwith "Failed to eval arithmetic expression"


        | Ok (LambdaEx (EVALCOMP result)) -> 
            /// result itself is an expression. It can give us either Literal or Variable
            /// so if it is literal we return literal, if this is expression of variable we perform search for each variable
            match result with
            | Comparison (Variable (IdString a), sign, Variable (IdString b)) ->
                let evaluatedA =  eval env (Ok (lambdaVar a))
                let evaluatedB =  eval env (Ok (lambdaVar b))
                match evaluatedA , evaluatedB with
                | Ok (LambdaEx (Literal a)), Ok (LambdaEx (Literal b)) -> 
                    match Comparison (Literal a, sign, Literal b) with
                    | EVALARITHM result -> Ok (LambdaEx result)
                    | _ -> failwith "failed to calculate comparison"
                | Ok (LambdaEx a), Ok (LambdaEx b) -> Ok (LambdaEx (Comparison (a, sign, b)))
                | _ -> failwith "Failed to evaluate Comparison from the environment"
                //| _ -> failwith "Cannot Evaluate"
            
            | Literal (x) -> Ok (LambdaEx (Literal x))
            | _ -> failwith "Failed to eval comparison expression"
        // if lambda is given save the relevant information into the closure and update the evironment 
        | Ok (LambdaT (id, body)) -> 
            if (Map.isEmpty env ) then Ok (ClosureT (id, body, env)) 
                else 
                    match eval env (Ok body) with
                    | Error msg -> Error msg
                    | Ok (body) -> Ok (ClosureT (id,body, env))
                    // Check the rule below, why will this never be applied??
                    //| _ -> Error "Wrong Lambda Applied"
        // if beta reduction is given, evaluate id & body and then perform beta reduction
        | Ok (BetaReductionT (LambdaT(id, body), LambdaEx(exp))) -> 
            let evaluatedLambdaEx = eval env (Ok (LambdaT(id, body)))
            match evaluatedLambdaEx with
            | Ok (ClosureT (arg, body, evalenv)) ->
                // in order to perform beta reduction with expression we need to match for variable & literals before further evaluation
                // LambdaEx includes beta-reduction cases
                // if value passed by expression it's already evaluated
                // pass expression as a term to substitue variable x
                // replace the argument
                // if variable is given then transform it to lambda and carry on as before
                // if Literal is given -> calculate
                let evaluatedValue = LambdaEx(exp)
                //let newEnv = (arg, evaluatedValue)::evalenv @ env
                let newEnv = evalenv.Add (arg, evaluatedValue)
                eval newEnv (Ok body)
            | _ -> failwith "cannot perform beta reduction"
                // return evaluated reduction for futher calulations
                // perform arthmetics and return LambdaEx!!
             // inputs like (a a) are expected. If (lambda a) is passed the expression will be evaluated as a beta reduction
             // Further match if func is an arithmetic expression and value is a literal therefore we may calculate the value  
        // Do I need Application
        | Ok (ApplicationT (func, value)) ->
            let evaluatedFunc = eval env (Ok func)
            match evaluatedFunc with
            | Ok (ClosureT (arg, body, evaluatedEnv)) ->
                let evaluatedValue = eval env (Ok value)
                match evaluatedValue with
                | Ok (ClosureT (valueArg, valueBody, valueEnv)) ->
                    let newEnv = valueEnv.Add (arg, (ClosureT (valueArg, valueBody, valueEnv)))
                    eval newEnv (Ok body)
                | _ -> 
                    let msg = sprintf "Failed to evaluate Application, %A is given" inp
                    Error msg
            | _ -> 
                match eval env (Ok func), eval env (Ok value) with
                | Error msg, Error _ -> Error msg
                | Ok func, Ok exp -> Ok (ApplicationT (func, exp))
                | _ -> 
                    let msg = sprintf "Failed to evaluate Application, %A is given" inp
                    Error msg
            
        | Ok (ClosureT (arg, body, oldenv)) -> Ok (ClosureT (arg, body, oldenv))
        | _ -> 
            let msg = sprintf "This Error Should not Appear, input is %A" inp
            Error msg
    innerFn



let LambdaTypeToString =
    let okToStr (inp: Result<string list,string>): string list =
        match inp with
        | Ok str -> str
        | Error msg -> [msg]

    let (|SIGN2STR|_|) = 
        let innerFn inp = 
            match inp with
            | Add -> Some "+"
            | Subtract -> Some "-"
            | Multiply -> Some "*"
            | Divide -> Some "/"
        innerFn

    let rec innerFn (inp: Result<LambdaType,string>) = 
        match inp with
        | Ok (LambdaT (arg, body)) -> 
            match innerFn (Ok body) with
            | Error msg -> Error msg
            | Ok body -> Ok ("lambda " :: arg :: "." :: body)
        | Ok (ClosureT (arg, body, env)) -> 
            match innerFn (Ok body) with
            | Error msg -> Error msg
            | Ok body -> Ok ("lambda " :: arg :: "." ::  body)
        | Ok (LambdaEx (Variable(IdString arg))) ->Ok [arg]
        | Ok (LambdaEx (Arithmetic(Variable (IdString x), SIGN2STR sign, Variable(IdString y)))) -> Ok [x + sign + y]
        | Ok (ApplicationT (exp1, exp2)) -> 
            match innerFn (Ok exp1),innerFn (Ok exp2)  with
            | Error msg, Error _ -> Error msg
            | Ok exp1, Ok exp2 -> Ok (["("] @ (exp1) @ [" "] @ (exp2) @ [")"])
            | _ -> Error "Failed to print"
        | Ok (BetaReductionT (lambda, exp)) ->
            match innerFn (Ok lambda), innerFn (Ok exp) with
            | Error msg, Error _ -> Error msg
            | Ok lambda, Ok exp -> Ok (["("] @ (lambda) @ [")" ; " "] @ (exp))
            | _ -> Error "Failed to print"
        | _ -> Error "Failed to print Lambda"
    (innerFn)
        >> okToStr
            >> String.concat ("") 


    

// convert to back to Ast

let toAst = 
    let innerFn (inp: Result<LambdaType,string>): Result<Ast,string> = 
        match inp with
        | Ok (LambdaEx (Literal exp)) -> Ok (Expression (Literal exp))
        | Ok closure -> 
            //print closure
            let lambdaString = LambdaTypeToString (Ok closure)
            let msg = sprintf "No valid expression supplied for lambda calculation. Evaluated pure lambda is: %A" lambdaString
            Error msg
        | _ -> 
            let msg = sprintf "toAst: Invalid match case with received input, %A" inp
            Error msg
    innerFn

// Final function to evaluate the output
let displayLambda: Result<Ast,string> -> string =
    parse
        >> eval Map.empty
            >> LambdaTypeToString

let lambda: Result<Ast,string> -> Result<Ast,string> = 
    parse
        >> eval Map.empty
            >> toAst






//////////////////////////////////////////////////////////////////
//////////////////////////                  //////////////////////
//////////////////////////      TESTS       //////////////////////
//////////////////////////                  //////////////////////
//////////////////////////////////////////////////////////////////

//  Test: lambda x . (x x)
// let inp1 = Function (None, IdString "x", FuncApp(Expression (Variable (IdString "x")), Expression (Variable (IdString "x"))))
// let inp2 = FuncApp (inp1, Function (None, IdString "y", Expression (Variable (IdString "y"))))

// // lambda x . x + x
// let inpAdd = Function(None, IdString "x", Expression( Arithmetic ((Variable (IdString "x")), Add, (Variable(IdString "x")))))
// // lambda z . z 
// let inp4 = Function (None, IdString "z",Expression (Variable (IdString "z")))
// // (Lambda x . Lambda y . x) Lambda z . z
// let inp3 = FuncApp (Function (None, IdString "x",Function (None, IdString "y", Expression (Variable (IdString "x")))), inp4)
// let inp5 = FuncApp (inpAdd, inp4)
// // print <| evaluateLamda inp1
// // print <| evaluateLamda inp2
// print <| parse inp3
// //print <| LambdaTypeToString (parse inp5)
// print <| eval Map.empty (parse inp3)
// print <| displayLambda inp3



// ///////////////////////////////////////////////////////////////////////
// ////////////////     Arithmetics Evaluation Test     //////////////////
// /// ///////////////////////////////////////////////////////////////////



// let testInp1 = lambdaVar "b"
// let testInp2 = LambdaEx (Arithmetic (Variable (IdString "b"), Add, Variable (IdString "a")))
// let testInp3 = LambdaEx (Arithmetic (Literal (Int 1), Add, Literal (Int 1)))
// let testInp4 = LambdaEx (Arithmetic (Literal (Int 1), Subtract, Literal (Int 1)))
// let testInp5 = LambdaEx (Arithmetic (Literal (Double 1.1), Subtract, Literal (Double 1.2)))
// let testInp6 = LambdaEx (Arithmetic (Literal (String "Hello "), Add, Literal (String "World")))
// let testInp7 = LambdaEx (Arithmetic (Literal (Double 1.1), Multiply, Literal (Double 1.2)))


// let testInp8 = LambdaEx (Comparison (Literal (Double 1.1), Lt, Literal (Double 1.2)))
// let testInp9 = LambdaEx (Comparison (Literal (Double 1.1), Eq, Literal (Double 1.2)))
// let testInp10 = LambdaEx (Comparison (Literal (String "Hey"), Eq, Literal (String "Hey")))




// let environment = Map.ofList ["b", lambdaVar "c" ; ("a", lambdaVar "a")]

// print <| eval environment testInp1
// print <| eval environment testInp2
// print <| eval environment testInp3
// print <| eval environment testInp4
// print <| eval environment testInp5
// print <| eval environment testInp6
// print <| eval environment testInp7
// print <| eval environment testInp8
// print <| eval environment testInp9
// print <| eval environment testInp10


// print "Final Test"
// print <| eval (Map.ofList ["x", LambdaEx(Literal(Double -2.56))]) (LambdaEx (Arithmetic(Variable (IdString "x"), Add, Variable (IdString "x"))))

