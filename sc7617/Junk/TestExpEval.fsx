

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
    // replace by beta reduction
    | ApplicationT of LambdaType * LambdaType
    | BetaReductionT of LambdaType * LambdaType
    //| BetaReducion of LambdaType * LambdaType

and Env = (string*LambdaType) list    






//////////////////////
/// Instructions//////
/// //////////////////

//  How to parse expression
/// expression can be:
///  -> Variable
///  -> Arithmetics  for example: Expression (Arithmetics (Expression(Variable ("a")), Add, Expression(Variable ("b"))))
///  -> Comaparison for example:  Expression (Comparison (Expression(Variable ("a")), More, Expression(Variable ("b"))))
///  -> Literal for example: 1, 2, "I wanna go to bed:("
///  Everything above must be able to be replaced by the incoming expressions and also everything must be aple to be saved in 
/// the environment
/// 
/// 
/// How can we calculate the expressions above??
/// Suppose we have the input:  Expression (Arithmetics (Expression(Variable ("a")), Add, Expression(Variable ("b"))))
/// which is the same as a + b, how do we evaluate it??
/// Evaluate at the seperate stage. Check if the expression can be evaluated at this stage, for example 2 + 2
/// cannot be reduced or evaluated further -> we can make it 4
/// 
/// The plan is the following
/// match inp with
/// | Lambda expression ->
///     match expression with
///     | Variable -> 
///             search_environment_and_replace
///             |> calculate
///     | Arithmetics (exp1, sign, exp2) -> 
///             let exp1Evaluated = eval env exp1
///             let exp2Evaluated = eval env exp2
///             evaluated match_arithmetics_built-in_funcion exp1Evaluated exp2Evaluated
///     | Compare (exp1, sign, exp2) -> 
///             let exp1Evaluated = eval env exp1
///             let exp2Evaluated = eval env exp2
///             evaluated match_comparison_built-in_funcion exp1Evaluated exp2Evaluated
///     | Literal n -> Literal
///     | _ -> failwith "expression cannot be evaluated"
 


 /// Helpful functions to match:
 /// LambdaEx Variable
 /// LambdaEx Literal
 /// LambdaEx Arithmetics
 /// 

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






// Evaluate Comparison!!!!
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



let rec eval (env: Env) (inp: LambdaType): LambdaType =
    let innerFn =  
        match inp with
        | LAMBDAVAR variable ->
            match List.tryFind (fun (aName, term) -> aName = variable) env with
            | Some (_, term) -> term
            | None -> failwith "Couldn't find a term by name"
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
        //| LambdaEx (Comparison (EVALCOMP result)) ->
            // REPLACE FOR SOMETHING MORE EFFIECIENT
            
    // make sure this variable has been declared before and therefore perform search through the environment 
    innerFn 








////////////////////////////////
/// LET'S WRITE SOME TESTS /////
/// ///////////////////////////

//let lambdafunc = LambdaT ()

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




let environment = ["b", lambdaVar "c" ; ("a", lambdaVar "a")]

// print <| eval environment testInp1
// print <| eval environment testInp2
// print <| eval environment testInp3
// print <| eval environment testInp4
// print <| eval environment testInp5
// print <| eval environment testInp6
// print <| eval environment testInp7
print <| eval environment testInp8
print <| eval environment testInp9
print <| eval environment testInp10


print "Final Test"
print <| eval ["x", LambdaEx(Literal(Double -2.56))] (LambdaEx (Arithmetic(Variable (IdString "x"), Add, Variable (IdString "x"))))