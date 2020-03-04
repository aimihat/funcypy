module Lambda

/// Helpful functions for debugging
let print x = printf "%A \n" x
let printPipe x = printf "%A \n" x ; x





// AST definition, can be further expanded for more language features

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
    | LambdaApp of LambdaType * LambdaType
    | LambdaCond of LambdaType * LambdaType * LambdaType option

and Env =  Map<string, LambdaType>





//////////////////////////////////////////////////////////////////
//////////////////////////                  //////////////////////
//////////////////////////   PARSING        //////////////////////
//////////////////////////                  //////////////////////
//////////////////////////////////////////////////////////////////

// Match lambda -> if input is lambda expression it returs argument and body, makes parser a lot cleaner
let (|LAMBDAMATCH|_|) (inp: Ast): (string*Ast) option = 
    match inp with
    | Function (None, IdString arg, body) -> Some (arg, body) | _ -> None // used to be failwith

// same idea as match lambda 
let (|VARIABLEMATCH|_|) inp =
    match inp with
    | Expression(Variable(IdString arg)) -> Some (Variable(IdString arg)) | _ -> None


// Parsing to make lambda types, needed for environment evaluation. Original Ast needs to be modified
let parse: (Result<Ast,string> -> Result<LambdaType,string>) = 
    /// Validates if evaluated expression is Ok and returns expression
    /// Ideally failwith should never appear if expressions are parsed correctly
    /// For Error free code, validate can be replaced by Error handling in parse function
    /// However this would make parse function look too messy
    let validate (inp: Result<LambdaType,string>) = 
        match inp with
        | Error msg -> failwith msg
        | Ok exp -> exp

    // inner loop
    let rec innerFn inp = 
        match inp with
        // If error recieved from parsing
        | Error msg -> Error msg
        
        //  Function (Lambda)
        | Ok (LAMBDAMATCH (id, body)) -> 
            match innerFn (Ok body) with
            | Error msg -> Error msg
            | Ok exp ->  Ok (LambdaT (id , exp))

        // all expressions
        | Ok (Expression (exp)) -> Ok (LambdaEx (exp))

        // Function application evaluation with all allowed combinations
        | Ok (FuncApp (func, value)) ->
            match func, value with
            | LAMBDAMATCH (arg, body), LAMBDAMATCH(arg', body') -> Ok (LambdaApp(LambdaT(arg, validate (innerFn (Ok body))), LambdaT(arg', validate (innerFn (Ok body')))))
            | LAMBDAMATCH (arg, body), Expression (exp) -> Ok(LambdaApp(LambdaT(arg, validate (innerFn (Ok body))), LambdaEx (exp)))
            | VARIABLEMATCH arg , Expression (exp) -> Ok (LambdaApp (LambdaEx(arg), LambdaEx exp))
            | FuncApp (func', value'), _ -> Ok (LambdaApp(validate (innerFn (Ok func)), validate (innerFn (Ok value))))
            | VARIABLEMATCH arg, FuncApp (func', value') -> Ok (LambdaApp (LambdaEx(arg), validate (innerFn (Ok value))))
            | _ -> 
                let msg = sprintf "Function Application failed to match, received Function Application is %A" inp
                Error msg
        
        // Conditional evaluation
        | Ok (Conditional (exp, ast, Some ast')) ->
            match Some ast' with
            // there is not else statement
            | None -> 
                Ok (LambdaCond(validate (innerFn (Ok (Expression exp))), validate (innerFn (Ok ast)), None))
            // there is an else statement
            | Some ast' -> 
                Ok (LambdaCond(validate (innerFn (Ok (Expression exp))), validate (innerFn (Ok ast)), Some (validate (innerFn (Ok ast')))))
                
        // If invalid expression supplied return Error
        | _ -> Error "Lambda Parsing Error"
    innerFn


//////////////////////////////////////////////////////////////////
//////////////////////////                  //////////////////////
//////////////////////////   ARITHMETICS    //////////////////////
//////////////////////////                  //////////////////////
//////////////////////////////////////////////////////////////////

/// Match lambda Variable is good partial active pattern to use since
/// Since it makes your code cleaner
let (|LAMBDAVAR|_|) =
    let innerFn inp = 
        match inp with
        | LambdaEx (Variable (IdString a)) -> Some a
        | _ -> None
    innerFn

/// This Partial Active Patter is used to make double from integer, to use fewer lines of code in the future
let (|MAKEDOUBLE|_|) = 
    let innerFn (inp: Ex): Ex option = 
        match inp with
        | Literal (Int a) -> Some (Literal (Double (float a)))
        | Literal (Double a) -> Some (Literal (Double a))
        | _ -> 
            let err = sprintf "Failed to get double, %A is given" inp
            failwith err
    innerFn

/// Performs addition
/// Performs float addition if one of the arguments is float -> exploating MAKEDOUBLE
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

// same for subtraction
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

/// Mutliplication
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

/// Division with condition to avoid division by zero
let (|DIVIDE|_|) = 
    let someLit x = Some (Literal x) 
    let innerFn ((n1:Ex), (n2: Ex)): Ex option = 
        match (n1, n2) with
        | (Literal (Int a), Literal (Int b)) when b <> 0 -> someLit (Int(a / b))
        | (MAKEDOUBLE (Literal (Double a)), MAKEDOUBLE (Literal (Double b))) -> someLit (Double (a / b))
        | _ -> 
            let error = sprintf "Division Failed, %A is given " (n1, n2)
            failwith error
    innerFn

// Declare some type wrapping to save later typing in EVALARITHM
let lambdaVar x = LambdaEx (Variable (IdString x))
let ExVar x = Variable (IdString x)





/// Evaluate all the arithmetics
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
            | Variable (IdString a), Variable(IdString b) -> Some (Arithmetic (ExVar a, sign, ExVar b))
            | Variable (IdString a), Literal (lit) -> Some (Arithmetic (ExVar a, sign, Literal (lit)))
            | Literal (lit), Variable (IdString a)-> Some (Arithmetic (Literal (lit), sign, ExVar a))
            | ADD result when sign == Add -> Some (result)
            | SUB result when sign == Subtract -> Some (result)
            | MULT result when sign == Multiply -> Some (result)
            | DIVIDE result when sign == Divide -> Some (result)           
            | _ -> failwith "sign is not recognised"
        | _ -> None
    innerFn

// make a function to use active pattern locally in eval function
let evalArithm inp =
    match inp with
    | EVALARITHM result -> result
    | _ -> failwith "Arithmetics cannot be evaluated"



/////////////////    Comparison Evaluation     ///////////////////

// Repeat same idea as before with Comparisons
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
            match exp1,  exp2 with
            | Variable (IdString a), Variable(IdString b) -> Some (Comparison (ExVar a, sign, ExVar b))
            | Variable (IdString a) , Literal lit -> Some (Comparison (ExVar a, sign, Literal lit))
            | Literal lit , Variable (IdString a) -> Some (Comparison (Literal lit, sign, ExVar a))
            | EQUAL result when sign == Eq -> Some (result)
            | NEQUAL result when sign == Ne -> Some (result)
            | LTHAN result when sign == Lt -> Some (result)           
            | GTHAN result when sign == Gt -> Some (result)
            | LEQTHAN result when sign == Le -> Some (result)
            | GEQTHAN result when sign == Ge -> Some (result)
            | _ -> failwith "sign is not recognised"
        | _ -> None
    innerFn

let evalCompare inp =
    match inp with
    | EVALCOMP result -> result
    | _ -> failwith "Comparison cannot be evaluated"

//////////////       END COMPARISON EVALUATION          ///////////////////


//////////////////////////////////////////////////////////////////
//////////////////////////                  //////////////////////
//////////////////////////   EVALUATION     //////////////////////
//////////////////////////                  //////////////////////
//////////////////////////////////////////////////////////////////

let rec eval (env: Env) (inp: Result<LambdaType,string>): Result<LambdaType,string> =
    let innerFn =  
        match inp with 
        
        // This the key function of pure lambda evalution, when we evaluating env it searches Env for value a for this variable
        // allows beta reduction and application
        | Ok (LambdaEx (Variable (IdString exp))) -> 
            match Map.tryFind exp env with
                | Some term -> Ok term
                | None -> Error "Couldn't find a Variable in the environment"

        // evaluate expression that where Partial Active Patterns plays their roles
        | Ok (LambdaEx (EVALARITHM result)) -> 
            /// result itself is an expression. It can give us either Literal or Variable
            /// so if it is literal we return literal, if this is expression of variable we perform search for each variable
            match result with
            | Arithmetic (Variable (IdString a), sign, Variable (IdString b)) ->
                let evaluatedA =  eval env (Ok (lambdaVar a))
                let evaluatedB =  eval env (Ok (lambdaVar b))
                match evaluatedA , evaluatedB with
                | Error msg, Error msg' -> Error msg
                | Ok (LambdaEx (Literal (a))), Ok (LambdaEx (Literal b)) ->
                    // replace by newly defined function 
                    match Arithmetic (Literal a, sign, Literal b) with
                    | EVALARITHM result -> Ok (LambdaEx result)
                    | _ -> failwith "failed to calculate arithmetic"
                | Ok (LambdaEx a), Ok (LambdaEx b) -> Ok (LambdaEx (Arithmetic (a, sign, b)))
                | _ -> Error "Failed to evaluate Arithmetic from the environment"

            // go through all possible scenarious to have better pure lambda evaluation
            | Arithmetic (Variable(IdString a), sign, Literal(lit)) ->
                let evaluatedA =  eval env (Ok (lambdaVar a))
                match evaluatedA with
                | Error msg-> Error msg
                | Ok (LambdaEx (Literal (a))) ->
                    Ok (LambdaEx (evalArithm (Arithmetic(Literal a, sign, Literal lit))))
                | Ok (LambdaEx (Variable (a))) ->
                    Ok (LambdaEx (Arithmetic (Variable a, sign, Literal lit)))
                | _ -> Error "failed to evaluate Arithmetic (Varible, sign, Literal)"
            
            | Arithmetic (Literal(lit) , sign, Variable(IdString a)) ->
                let evaluatedA =  eval env (Ok (lambdaVar a))
                match evaluatedA with
                | Error msg-> Error msg
                | Ok (LambdaEx (Literal (a))) ->
                    Ok (LambdaEx (evalArithm (Arithmetic(Literal lit , sign, Literal a))))
                | Ok (LambdaEx (Variable (a))) ->
                    Ok (LambdaEx (Arithmetic (Literal lit , sign, Variable a)))
                | _ -> Error "failed to evaluate Arithmetic (Literal , sign, Varible)"
             
            // The desired output, Literal, means our arithmetics expression evaluated successfully
            | Literal (x) -> Ok (LambdaEx (Literal x))
            | _ ->
                let msg = sprintf "Failed to eval arithmetic expression, the input give is %A" inp
                Error msg

        // evaluate comparison
        | Ok (LambdaEx (EVALCOMP result)) -> 
            /// result itself is an expression. It can give us either Literal or Variable
            /// so if it is literal we return literal, if this is expression of variable we perform search for each variable
            match result with
            | Comparison (Variable (IdString a), sign, Variable (IdString b)) ->
                let evaluatedA =  eval env (Ok (lambdaVar a))
                let evaluatedB =  eval env (Ok (lambdaVar b))
                match evaluatedA , evaluatedB with
                | Error msg, Error msg' -> Error msg
                | Ok (LambdaEx (Literal a)), Ok (LambdaEx (Literal b)) -> 
                    match Comparison (Literal a, sign, Literal b) with
                    | EVALCOMP result -> Ok (LambdaEx result)
                    | _ -> failwith "failed to calculate comparison"
                | Ok (LambdaEx a), Ok (LambdaEx b) -> Ok (LambdaEx (Comparison (a, sign, b)))
                | _ -> failwith "Failed to evaluate Comparison from the environment"
                //| _ -> failwith "Cannot Evaluate"
            
            | Comparison (Variable(IdString a), sign, Literal(lit)) ->
                let evaluatedA =  eval env (Ok (lambdaVar a))
                match evaluatedA with
                | Error msg-> Error msg
                | Ok (LambdaEx (Literal (a))) ->
                    Ok (LambdaEx (evalCompare (Comparison(Literal a, sign, Literal lit))))
                | Ok (LambdaEx (Variable (a))) ->
                    Ok (LambdaEx (Comparison (Variable a, sign, Literal lit)))
                | _ -> Error "failed to evaluate Comparison (Varible, sign, Literal)"
            
            | Comparison (Literal(lit) , sign, Variable(IdString a)) ->
                let evaluatedA =  eval env (Ok (lambdaVar a))
                match evaluatedA with
                | Error msg-> Error msg
                | Ok (LambdaEx (Literal (a))) ->
                    Ok (LambdaEx (evalCompare (Comparison(Literal lit , sign, Literal a))))
                | Ok (LambdaEx (Variable (a))) ->
                    Ok (LambdaEx (Comparison (Literal lit , sign, Variable a)))
                | _ -> Error "failed to evaluate Comparison (Literal , sign, Varible)"


            | Literal (x) -> Ok (LambdaEx (Literal x))
            | _ -> failwith "Failed to eval comparison expression"

        // if lambda is given save the relevant information into the closure and update the evironment
        // Lambda may also arrive with conditional which is matched as a tweak
        | Ok (LambdaT (arg, body)) ->
            if (Map.isEmpty env ) then 
                match body with
                | LambdaCond (cond, iftrue, orelse) -> eval (env.Add(arg, LambdaEx(Variable(IdString arg)))) (Ok body)
                | _ -> Ok (ClosureT (arg, body, env)) 
                else 
                    match eval env (Ok body) with
                    | Error msg -> Error msg
                    | Ok (body) -> Ok (ClosureT (arg,body, env))

        // Evaluate Lambda appllication
        | Ok (LambdaApp (func, value)) ->
            // evaluate function side
            let evaluatedFunc = eval env (Ok func)
            match evaluatedFunc with
            // match with what we expect to see
            //| Error msg -> Error msg
            | Ok (ClosureT (arg, body, evaluatedEnv)) ->
                //evaluate value side
                let evaluatedValue = eval env (Ok value)
                // match with expected values
                match evaluatedValue, value with
                //| Error msg, _ -> Error msg
                // if function will be evaluated to pure lambda
                | Ok (ClosureT (valueArg, valueBody, valueEnv)), _ ->
                    // if lambda is given replace all occurance with lambda
                    let newEnv = valueEnv.Add (arg, ClosureT (valueArg, valueBody, valueEnv))
                    eval newEnv (Ok body)
                | _ , (LambdaEx (exp)) -> 
                    // if expression given replace with expression -> beta reduction
                    let newEnv = evaluatedEnv.Add (arg, LambdaEx (exp))
                    eval newEnv (Ok body)
                | _ ->
                    let msg = sprintf "Failed to evaluate Application, %A is given" inp
                    Error msg
            // if function received
            | Ok (LambdaEx(Variable arg)) ->
                let evaluatedValue = eval env (Ok value)
                match evaluatedValue with
                //| Error msg -> Error msg
                | Ok (LambdaEx exp) -> Ok (LambdaApp(LambdaEx (Variable arg), LambdaEx exp))
                | _ ->
                    let msg = sprintf "Failed to Evaluate Value in Application when LambdaEx is expected, but %A is given" inp
                    Error msg

            | _ -> 
                let msg = sprintf "Failed to evaluate Application, %A is given" inp
                Error msg

        // evaluate condition, simply ger values for true and false and return
        | Ok (LambdaCond(cond, iftrue, orelse)) ->
            let evaluatedcond = eval env (Ok cond)
            match evaluatedcond, orelse with
            | Error msg, _ -> Error msg
            | Ok (LambdaEx (Literal (Bool true ))), _ -> (eval env (Ok iftrue))
            | Ok (LambdaEx (Literal (Bool false ))), Some orelse -> (eval env (Ok orelse))           
            | _ -> 
                let msg = sprintf "Lambda Conditional Failed to evaluate, with received input %A" inp
                Error msg

        | Ok (LambdaEx (Literal (lit)))-> Ok (LambdaEx(Literal (lit)))    
        | Ok (ClosureT (arg, body, oldenv)) -> Ok (ClosureT (arg, body, oldenv))
        | _ -> 
            let msg = sprintf "This Error Should not Appear, input is %A" inp
            Error msg
    innerFn


// This function is used to display evaluated pure lambda expression
let LambdaTypeToString =
    let okToStr (inp: Result<string list,string>): string list =
        match inp with
        | Ok str -> str
        | Error msg -> [msg]

    // recognise all the signs correctly
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
        | Ok (LambdaEx (Arithmetic(Variable (IdString x), SIGN2STR sign, Literal (Int y)))) -> Ok [x + sign + string(y)]
        | Ok (LambdaEx (Arithmetic(Variable (IdString x), SIGN2STR sign, Literal (Double y)))) -> Ok [x + sign + string(y)]
        | Ok (LambdaEx (Arithmetic(Variable (IdString x), SIGN2STR sign, Literal (Bool y)))) -> Ok [x + sign + string(y)]
        | Ok (LambdaEx (Arithmetic(Variable (IdString x), SIGN2STR sign, Literal (String y)))) -> Ok [x + sign + y]

        | Ok (LambdaEx (Arithmetic(Literal (Int x), SIGN2STR sign, Variable (IdString y)))) -> Ok [string(x) + sign + y]
        | Ok (LambdaEx (Arithmetic(Literal (Double x), SIGN2STR sign, Variable (IdString y)))) -> Ok [string(x) + sign + y]
        | Ok (LambdaEx (Arithmetic(Literal (Bool x), SIGN2STR sign, Variable (IdString y)))) -> Ok [string(x) + sign + y]
        | Ok (LambdaEx (Arithmetic(Literal (String x), SIGN2STR sign, Variable (IdString y)))) -> Ok [x + sign + y]


        | Ok (LambdaApp (exp1, exp2)) -> 
            match innerFn (Ok exp1),innerFn (Ok exp2)  with
            | Error msg, Error _ -> Error msg
            | Ok exp1, Ok exp2 -> Ok (["("] @ (exp1) @ [" "] @ (exp2) @ [")"])
            | _ -> Error "LambdaApp -> String conversion failed"
        
        


        | _ -> 
            let msg = sprintf "Failed to print Lambda, %A received" inp 
            Error msg
    (innerFn)
        >> okToStr
            >> String.concat ("") 


    

// convert back to Ast
let toAst = 
    let innerFn (inp: Result<LambdaType,string>): Result<Ast,string> = 
        match inp with
        | Error msg -> Error msg
        | Ok (LambdaEx (Literal exp)) -> Ok (Expression (Literal exp))
        | Ok closure -> 
            //print closure
            let lambdaString = LambdaTypeToString (Ok closure)
            let msg = sprintf "No valid expression supplied for lambda calculation. Evaluated pure lambda is: %A" lambdaString
            Error msg
    innerFn

// Final function to evaluate the output (used for debugging)
let displayLambda: Result<Ast,string> -> string =
    parse
        >> eval Map.empty
            >> LambdaTypeToString

let lambda: Result<Ast,string> -> Result<Ast,string> = 
    parse
        >> eval Map.empty
            >> toAst
