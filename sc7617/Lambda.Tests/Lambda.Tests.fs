module Lambda.Tests
open System
//open ParseHelpers

open Expecto
open FsCheck
open Lambda

let print x = printf "%A \n" x
let printPipe x = printf "%A \n" x; x


module Gen =

    type GenIntList = GenIntList of int list
    let IntList = 
        Gen.choose(-999,999)
        |> Gen.listOf
        |> Arb.fromGen
        |> Arb.convert GenIntList (fun (GenIntList l) -> l)

    type GenArithmetic = GenArithmetic of Result<Lambda.Ast,string>
    let ArithmeticAddEx = 
        let generateInt = Gen.choose(-999,999)
        let optSign = Gen.elements [Lambda.Add ; Lambda.Subtract ; Lambda.Multiply ; Lambda.Divide]
        //let arithEx n1 sign n2 = (Lambda.Arithmetic(Lambda.Literal(Lambda.Int n1), sign, Lambda.Literal(Lambda.Int n2)))
        Gen.map3 (fun exp1 sign exp2 -> 
        Ok (Lambda.Expression(Lambda.Arithmetic(Lambda.Literal(Lambda.Int exp1), sign, Lambda.Literal(Lambda.Int exp2)))) ) generateInt optSign generateInt
        |> Arb.fromGen
        |> Arb.convert GenArithmetic (fun (GenArithmetic exp) -> exp)

    let addToConfig config =
        let addedTypes = [
                typeof<GenArithmetic>.DeclaringType
            ]
        { config with arbitrary = addedTypes @ config.arbitrary}
        
        

[<AutoOpen>]
module Auto =
    let private config = Gen.addToConfig FsCheckConfig.defaultConfig
    //let private config100 = Gen.addToConfig config
    let testProp name = testPropertyWithConfig config name
    let ptestProp name = ptestPropertyWithConfig config name
    let ftestProp name = ftestPropertyWithConfig config name
    let etestProp stdgen name = etestPropertyWithConfig stdgen config name

module Tests = 
    [<Tests>]
    let topicTests =
        testList "topic" [
            testProp "Arithmetics Generated Correctly" (fun (Gen.GenArithmetic (exp)) ->
                (   
                    // printPipe (exp) |> ignore
                    match (exp) with
                    | Ok (Lambda.Expression(Lambda.Arithmetic (exp1, sign, exp2))) -> Expect.isOk (Ok exp) 
                    | _ ->  Expect.isOk (Error "Incorrectly generated")
                )
            )
            testProp "Arithmetics Evaluated to Literal" (fun (Gen.GenArithmetic (exp)) ->
                (
                    // printPipe (lambda exp)
                    Expect.isOk (lambda exp)
                )
            )
            testCase "Test: (lambda x.x  y)" <| fun () ->
                (
                    // test case (lambda x.x) y -> y
                    let expected = Error (sprintf "No valid expression supplied for lambda calculation. Evaluated pure lambda is: %A" "y")
                    let inp = FuncApp (Function(None, IdString "x", Expression (Variable (IdString "x"))),Expression (Variable (IdString "y")))
                    Expect.equal (lambda (Ok inp)) expected  "(lambda x.x  y) Failed"
                )        
        ]

[<EntryPoint>]
let main argv =
    let expectoconfig = {defaultConfig with verbosity = Logging.LogLevel.Debug}
 
    runTests expectoconfig Tests.topicTests |> ignore
    
       
    printfn "press any key to terminate"
    Console.ReadKey() |> ignore
    0 // return an integer exit code