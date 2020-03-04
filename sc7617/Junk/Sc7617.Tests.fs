module Sc7617.Tests
open System
//open ParseHelpers

open Expecto
open FsCheck
open Lambda

let print x = printf "%A \n" x


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


module Gen =

    type GenIntList = GenIntList of int list
    let IntList = 
        Gen.choose(-999,999)
        |> Gen.listOf
        |> Arb.fromGen
        |> Arb.convert GenIntList (fun (GenIntList l) -> l)

    type GenArithmetic = GenArithmetic of Ast
    let ArithmeticAddEx = 
        let generateInt = Gen.choose(-999,999)
        let optSign = Gen.elements [Add ; Subtract ; Multiply ; Divide]
        let arithEx n1 sign n2 = Expression(Arithmetic(Literal(Int n1), sign, Literal(Int n2)))
        Gen.map3 arithEx generateInt optSign generateInt
        |> Arb.fromGen
        |> Arb.convert GenArithmetic (fun (GenArithmetic exp) -> exp)


[<AutoOpen>]
module Auto =
    let private config = FsCheckConfig.defaultConfig
    //let private config100 = Gen.addToConfig config
    let testProp name = testPropertyWithConfig config name
    let ptestProp name = ptestPropertyWithConfig config name
    let ftestProp name = ftestPropertyWithConfig config name
    let etestProp stdgen name = etestPropertyWithConfig stdgen config name

module Tests = 
    [<Tests>]
    let topicTests =
        testList "topic" [
            testProp "Return is a Literal" (fun (Gen.GenArithmetic f) ->
                (Lambda.eval Map.empty f) // test
            )
        ]

[<EntryPoint>]
let main argv =
    let expectoconfig = {defaultConfig with verbosity = Logging.LogLevel.Debug}
 
    
    print <| Gen.IntList    
    printfn "press any key to terminate"
    Console.ReadKey() |> ignore
    0 // return an integer exit code