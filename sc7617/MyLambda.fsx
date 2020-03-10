

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

let parse: (Ast -> LambdaType) = 
    let rec innerFn inp = 
        match inp with
        //  Function (Lambda)
        | LAMBDAMATCH (id, body) -> LambdaT (id , (innerFn body))

        // all expressions
        | Expression (exp) -> LambdaEx (exp)

        // Invalid function application
        | FuncApp (Expression _, LAMBDAMATCH _) -> failwith "not acceptable match (variable lambda)"
        // Match conditionals and comparators
        | FuncApp (LAMBDAMATCH (id, body), Expression exp) -> BetaReductionT (LambdaT (id, (innerFn body)), innerFn (Expression exp))
        | FuncApp (func, exp) -> ApplicationT (innerFn func , innerFn exp)
        
        // Beta reduction if second argument passed is a value or variable
        // Application works the same ways as beta reductions
        // Beta reduction evaluation variable must not be LambdaT variable -> it should be declared as an Ex type
        // Beta reduction can only take input Lambdat (or ClosureT ? ) and Ex and after further reductions made, perform calculation  
        // or reduction -> the result might be an extension
        // illegal case for funcion applictation -> in the future replace EXPVAR by EXPMATCH, since any expression is not valid
        // Beta Reduction case
        
        // Evaluate alpha reduction

        // evaluatio beta reduction
        | _ -> failwith "Lambda Parsing Error"
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

let rec eval (env: Env) (inp: LambdaType) =
    let innerFn =  
        match inp with 
        // make sure this variable has been declared before and therefore perform search through the environment 
        | LambdaEx (Variable (IdString exp)) -> 
            match Map.tryFind exp env with
                | Some term -> term
                | None -> failwith "Couldn't find a Variable in the environment"

        | LambdaEx (EVALARITHM result) -> 
            /// result itself is an expression. It can give us either Literal or Variable
            /// so if it is literal we return literal, if this is expression of variable we perform search for each variable
            match result with
            | Arithmetic (Variable (IdString a), sign, Variable (IdString b)) ->
                let evaluatedA =  eval env (lambdaVar a)
                let evaluatedB =  eval env (lambdaVar b)
                match evaluatedA , evaluatedB with
                | LambdaEx (Literal (a)), LambdaEx (Literal b) -> 
                    match Arithmetic (Literal a, sign, Literal b) with
                    | EVALARITHM result -> LambdaEx result
                    | _ -> failwith "failed to calculate arithmetic"
                | LambdaEx (a), LambdaEx (b) -> LambdaEx (Arithmetic (a, sign, b))
                | _ -> failwith "Failed to evaluate Arithmetic from the environment"
                //| _ -> failwith "Cannot Evaluate"
            
            | Literal (x) -> LambdaEx (Literal x)
            | _ -> failwith "Failed to eval arithmetic expression"


        | LambdaEx (EVALCOMP result) -> 
            /// result itself is an expression. It can give us either Literal or Variable
            /// so if it is literal we return literal, if this is expression of variable we perform search for each variable
            match result with
            | Comparison (Variable (IdString a), sign, Variable (IdString b)) ->
                let evaluatedA =  eval env (lambdaVar a)
                let evaluatedB =  eval env (lambdaVar b)
                match evaluatedA , evaluatedB with
                | LambdaEx (Literal (a)), LambdaEx (Literal b) -> 
                    match Comparison (Literal a, sign, Literal b) with
                    | EVALARITHM result -> LambdaEx result
                    | _ -> failwith "failed to calculate comparison"
                | LambdaEx (a), LambdaEx (b) -> LambdaEx (Comparison (a, sign, b))
                | _ -> failwith "Failed to evaluate Comparison from the environment"
                //| _ -> failwith "Cannot Evaluate"
            
            | Literal (x) -> LambdaEx (Literal x)
            | _ -> failwith "Failed to eval comparison expression"
        // if lambda is given save the relevant information into the closure and update the evironment 
        | LambdaT (id, body) -> 
            if (Map.isEmpty env ) then ClosureT (id, body, env) else ClosureT (id, eval env body, env)

        // if beta reduction is given, evaluate id & body and then perform beta reduction
        | BetaReductionT (lambda, value) -> 
            let evaluatedLambdaEx = eval env lambda
            match evaluatedLambdaEx with
            | ClosureT (arg, body, evalenv) ->
                // in order to perform beta reduction with expression we need to match for variable & literals before further evaluation
                match value with
                // LambdaEx includes beta-reduction cases
                | LambdaEx (_)->
                    // if value passed by expression it's already evaluated
                    // pass expression as a term to substitue variable x
                    // replace the argument
                    // if variable is given then transform it to lambda and carry on as before
                    // if Literal is given -> calculate
                    let evaluatedValue = value
                    //let newEnv = (arg, evaluatedValue)::evalenv @ env
                    let newEnv = evalenv.Add (arg, evaluatedValue)
                    eval newEnv body
                | _ -> 
                    failwith "Invalid definition of Beta Reduction"
            | _ -> failwith "cannot perform beta reduction"
                // return evaluated reduction for futher calulations
                // perform arthmetics and return LambdaEx!!
             // inputs like (a a) are expected. If (lambda a) is passed the expression will be evaluated as a beta reduction
             // Further match if func is an arithmetic expression and value is a literal therefore we may calculate the value  
        // Do I need Application
        | ApplicationT (func, value) ->
            let evaluatedFunc = eval env func
            match evaluatedFunc with
            | ClosureT (arg, body, evaluatedEnv) ->
                let evaluatedValue = eval env value
                match evaluatedValue with
                | ClosureT (valueArg, valueBody, valueEnv) ->
                    let newEnv = valueEnv.Add (arg, evaluatedValue)
                    eval newEnv body
                | _ -> 
                    let msg = sprintf "Failed to evaluate Application, %A is given" inp
                    failwith msg
            | _ -> ApplicationT (eval env func, eval env value)
            
        | ClosureT (arg, body, oldenv) -> ClosureT (arg, body, oldenv)
        //| _ -> failwith "failed to evaluate the expression"y
    innerFn

let LambdaTypeToString =
    let rec innerFn (inp: LambdaType) = 
        match inp with
        | LambdaT (arg, body) -> "lambda " :: arg :: "." :: innerFn body
        | ClosureT (arg, body, env) -> "lambda " :: arg :: "." :: innerFn body
        | LambdaEx (Variable(IdString arg)) -> [arg]
        | ApplicationT (exp1, exp2) -> ["("] @ (innerFn exp1) @ [" "] @ (innerFn exp2) @ [")"]
        | BetaReductionT (lambda, exp) -> ["("] @ (innerFn lambda) @ [")" ; " "] @ (innerFn exp)
        | _ -> failwith "Failed to printLambda"
    innerFn
        >> String.concat ("") 


    

// convert to back to Ast

let toAst = 
    let innerFn (inp: LambdaType): Result<Ast,string> = 
        match inp with
        | LambdaEx exp -> Ok (Expression exp)
        | closure -> 
            let lambdaString = LambdaTypeToString closure
            let msg = sprintf "No valid expression supplied for lambda calculation. Evaluated pure lambda is: %A" lambdaString
            Error msg
    innerFn

// Final function to evaluate the output
let displayLambda: Ast -> string =
    parse
        >> eval Map.empty
            >> LambdaTypeToString

let lambda: Ast -> Result<Ast,string> = 
    parse
        >> eval Map.empty
            >> toAst






//////////////////////////////////////////////////////////////////
//////////////////////////                  //////////////////////
//////////////////////////      TESTS       //////////////////////
//////////////////////////                  //////////////////////
//////////////////////////////////////////////////////////////////

//  Test: lambda x . (x x)
let inp1 = Function (None, IdString "x", FuncApp(Expression (Variable (IdString "x")), Expression (Variable (IdString "x"))))
let inp2 = FuncApp (inp1, Function (None, IdString "y", Expression (Variable (IdString "y"))))

// lambda x . x + x
let inpAdd = Function(None, IdString "x", Expression( Arithmetic ((Variable (IdString "x")), Add, (Variable(IdString "x")))))
// lambda z . z 
let inp4 = Function (None, IdString "z",Expression (Variable (IdString "z")))
// (Lambda x . Lambda y . x) Lambda z . z
let inp3 = FuncApp (Function (None, IdString "x",Function (None, IdString "y", Expression (Variable (IdString "x")))), inp4)
let inp5 = FuncApp (inpAdd, inp4)
// print <| evaluateLamda inp1
// print <| evaluateLamda inp2
print <| parse inp3
//print <| LambdaTypeToString (parse inp5)
print <| eval Map.empty (parse inp3)
print <| displayLambda inp3



///////////////////////////////////////////////////////////////////////
////////////////     Arithmetics Evaluation Test     //////////////////
/// ///////////////////////////////////////////////////////////////////



let testInp1 = lambdaVar "b"
let testInp2 = LambdaEx (Arithmetic (Variable (IdString "b"), Add, Variable (IdString "a")))
let testInp3 = LambdaEx (Arithmetic (Literal (Int 1), Add, Literal (Int 1)))
let testInp4 = LambdaEx (Arithmetic (Literal (Int 1), Subtract, Literal (Int 1)))
let testInp5 = LambdaEx (Arithmetic (Literal (Double 1.1), Subtract, Literal (Double 1.2)))
let testInp6 = LambdaEx (Arithmetic (Literal (String "Hello "), Add, Literal (String "World")))
let testInp7 = LambdaEx (Arithmetic (Literal (Double 1.1), Multiply, Literal (Double 1.2)))


let testInp8 = LambdaEx (Comparison (Literal (Double 1.1), Lt, Literal (Double 1.2)))
let testInp9 = LambdaEx (Comparison (Literal (Double 1.1), Eq, Literal (Double 1.2)))
let testInp10 = LambdaEx (Comparison (Literal (String "Hey"), Eq, Literal (String "Hey")))




let environment = Map.ofList ["b", lambdaVar "c" ; ("a", lambdaVar "a")]

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

