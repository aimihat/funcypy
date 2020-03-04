module Lambda.Tests
open System
//open ParseHelpers

open Expecto
open FsCheck
open Lambda

let print x = printf "%A \n" x
let printPipe x = printf "%A \n" x; x


module Gen =

    // variable name generator used in further generators
    let variableName = Gen.map Char.ToString (Gen.elements(['a' .. 'z']))

    // Generate String
    let generateString =
        let alphaGen = Gen.elements ([ 'a'..'z'] @ ['A'..'Z'])
        let digitGen = Gen.elements(['0'..'9'])
        let alphaNumGen = Gen.oneof [alphaGen ; digitGen]
        let arrToStr a = a |> Array.map Char.ToString |> String.concat "" 
        Gen.arrayOf alphaNumGen 
        |> Gen.map arrToStr 

    // generate list of integers (to test that simple generators works correctly)
    type GenIntList = GenIntList of int list
    let IntList = 
        Gen.choose(-999,999)
        |> Gen.listOf
        |> Arb.fromGen
        |> Arb.convert GenIntList (fun (GenIntList l) -> l)

    // there is no overlap in Ast therefore I can use Ast declared in Lambda and avoid writing Lambda.Ast
    // Generate Integer Arithmetic expression which must be evaluated to the literal
    type GenArithLit = GenArithLit of Result<Ast,string>
    let ArithmeticAddEx = 
        let generateInt1 = Gen.choose(-999,-1)
        let generateInt2 = Gen.choose(1,999)
        let generateInt = Gen.oneof [generateInt1 ; generateInt2]
        let optSign = Gen.elements [Add ; Subtract ; Multiply ; Divide]
        //let arithEx n1 sign n2 = (Lambda.Arithmetic(Lambda.Literal(Lambda.Int n1), sign, Lambda.Literal(Lambda.Int n2)))
        Gen.map3 (fun exp1 sign exp2 -> 
        Ok (Expression(Arithmetic(Literal(Int exp1), sign, Literal(Int exp2)))) ) generateInt optSign generateInt
        |> Arb.fromGen
        |> Arb.convert GenArithLit (fun (GenArithLit exp) -> exp)
    
    // String Addition generator -> evaluated to a Literal String
    type GenStringAdd = GenStringAdd of Result<Ast,string>
    let stringAdd = 
        Gen. map2 (fun str1 str2 ->
        Ok (Expression(Arithmetic(Literal (String str1), Add, Literal(String str2))))) generateString generateString
        |> Arb.fromGen
        |> Arb.convert GenStringAdd (fun (GenStringAdd exp) -> exp)

    // Arithmetic Ast generator -> generates mix of variables and integers for pure lambda evaluation tests (Reusable)
    let genArithEx = 
        let generateInt = Gen.choose(-999,999)
        let optSign = Gen.elements [Add ; Subtract ; Multiply ; Divide]
        let varInt = Gen.map3 (fun exp1 sign exp2 -> 
            (Expression(Arithmetic(Variable(IdString exp1), sign, Literal(Int exp2)))) ) variableName optSign generateInt
        let intVar = Gen.map3 (fun exp1 sign exp2 -> 
            (Expression(Arithmetic(Literal(Int exp1), sign, Variable(IdString exp2)))) ) generateInt optSign variableName
        let intInt = Gen.map3 (fun exp1 sign exp2 -> 
            (Expression(Arithmetic(Literal(Int exp1), sign, Literal(Int exp2)))) ) generateInt optSign generateInt
        let varVar = Gen.map3 (fun exp1 sign exp2 -> 
            (Expression(Arithmetic(Variable(IdString exp1), sign, Variable(IdString exp2)))) ) variableName optSign variableName   
        Gen.oneof [varInt; intVar; intInt; varVar ]

    // Arbitatry generator of arithmetic expression. The only difference betweeen this one and the previous is that the previous one can be 
    // easialy resused when this one will require conversion which is hard to make
    type GenArbArithEx = GenArbArithEx of Result<Ast,string>
    let genOkArithEx = 
        let generateInt = Gen.choose(-999,999)
        let optSign = Gen.elements [Add ; Subtract ; Multiply ; Divide]
        let varInt = Gen.map3 (fun exp1 sign exp2 -> 
            Ok (Expression(Arithmetic(Variable(IdString exp1), sign, Literal(Int exp2)))) ) variableName optSign generateInt
        let intVar = Gen.map3 (fun exp1 sign exp2 -> 
            Ok (Expression(Arithmetic(Literal(Int exp1), sign, Variable(IdString exp2)))) ) generateInt optSign variableName
        let intInt = Gen.map3 (fun exp1 sign exp2 -> 
            Ok (Expression(Arithmetic(Literal(Int exp1), sign, Literal(Int exp2)))) ) generateInt optSign generateInt
        let varVar = Gen.map3 (fun exp1 sign exp2 -> 
            Ok (Expression(Arithmetic(Variable(IdString exp1), sign, Variable(IdString exp2)))) ) variableName optSign variableName   
        Gen.oneof [varInt; intVar; intInt; varVar ]
        |> Arb.fromGen
        |> Arb.convert GenArbArithEx (fun (GenArbArithEx exp) -> exp)

    // Generates arbitary comparison with Integers
    type GenCompEx = GenCompEx of Result<Ast,string>
    let genExComp = 
        let generateInt = Gen.choose(-999,999)
        let optSign = Gen.elements [Eq ; Ne ; Lt ; Gt ; Le ; Ge]
        Gen.map3 (fun exp1 sign exp2 -> 
            Ok (Expression(Comparison(Literal(Int exp1), sign, Literal(Int exp2)))) ) generateInt optSign generateInt
        |> Arb.fromGen
        |> Arb.convert GenCompEx (fun (GenCompEx exp) -> exp)

    // Generates conditional expressions, valid and not valid since the variable names in Lambda body must be declared in arguments. 
    // Explained further in tests
    type GenCond = GenCond of Result<Ast,string>            
    let genCond = 
        let generateInt = Gen.choose(-999,999)
        let optSign = Gen.elements [Eq ; Ne ; Lt ; Gt ; Le ; Ge]
        let compEx = Gen.map3 (fun exp1 sign exp2 -> 
            (Comparison(Literal(Int exp1), sign, Literal(Int exp2)))) generateInt optSign generateInt
        let condEx = Gen.map3 (fun comp exp1 exp2 -> 
                (Conditional(comp, exp1, Some exp2))) compEx genArithEx genArithEx
        Gen.map2 (fun arg body ->
            Ok (Function(None, IdString arg, body))) variableName condEx
        |> Arb.fromGen
        |> Arb.convert GenCond (fun (GenCond exp) -> exp)

    // Add aribitary generators to configuration
    let addToConfig config =
        let addedTypes = [
                typeof<GenArithLit>.DeclaringType
                typeof<GenStringAdd>.DeclaringType
                typeof<GenCompEx>.DeclaringType
                typeof<GenCond>.DeclaringType
                //typeof<GenCond>.DeclaringType


            ]
        { config with arbitrary = addedTypes @ config.arbitrary}
        
        
// Initial setup
[<AutoOpen>]
module Auto =
    let private config = Gen.addToConfig FsCheckConfig.defaultConfig
    //let private config100 = Gen.addToConfig config
    let testProp name = testPropertyWithConfig config name
    let ptestProp name = ptestPropertyWithConfig config name
    let ftestProp name = ftestPropertyWithConfig config name
    let etestProp stdgen name = etestPropertyWithConfig stdgen config name

// Run tests
module Tests = 
    [<Tests>]
    let topicTests =
        testList "Lambda Evaluations" [
            testProp "Arithmetics Generated Correctly" (fun (Gen.GenArithLit (exp)) ->
                (   
                    // printPipe (exp) |> ignore
                    match (exp) with
                    | Ok (Lambda.Expression(Arithmetic (exp1, sign, exp2))) -> Expect.isOk (Ok exp) 
                    | _ ->  Expect.isOk (Error "Incorrectly generated")
                )
            )
            testProp "Arithmetics Evaluate to a Literal" (fun (Gen.GenStringAdd (exp)) ->
                (   
                    // lambda evaluates to Ok value only if it evaluates to a literal
                    Expect.isOk (lambda exp)
                )
            )

            testProp "String Arithmetics Evaluate to a Literal" (fun (Gen.GenArithLit (exp)) ->
                (
                    Expect.isOk (lambda exp)
                )
            )
            // Betareduction 
            testCase "Test: (lambda x.x  y)" <| fun () ->
                (
                    // test case (lambda x.x) y -> y
                    let expected = Error (sprintf "No valid expression supplied for lambda calculation. Evaluated pure lambda is: %A" "y")
                    let inp = FuncApp (Function(None, IdString "x", Expression (Variable (IdString "x"))),Expression (Variable (IdString "y")))
                    Expect.equal (lambda (Ok inp)) expected  "(lambda x.x  y) Failed"
                )
            // This test case tests, general beta reduction -> replace all occurances of x in the expression and evaluate.
            testCase "(Lambda x. x + x) z " <| fun () ->
                (
                    let expected = Error (sprintf "No valid expression supplied for lambda calculation. Evaluated pure lambda is: %A" "z+z")
                    let inp = FuncApp (Function(None, IdString "x", Expression (Arithmetic((Variable (IdString "x")), Add, (Variable (IdString "x"))))),Expression (Variable (IdString "z")))
                    Expect.equal (lambda (Ok inp)) expected  "(lambda x. x + x)  z Failed"
                )
            testCase "(Lambda f. Lambda x. (f x)) Lambda y. y + 1 " <| fun() ->
                (
                    // For this tes since the input Ast is very long I will split it in several parts
                    let expected = Error (sprintf "No valid expression supplied for lambda calculation. Evaluated pure lambda is: %A" "lambda x.x+1")
                    let lambdaFnInner = Function(None, IdString "x", FuncApp(Expression(Variable(IdString "f")), Expression(Variable(IdString "x"))))
                    let lambdaFn = Function(None, IdString "f", lambdaFnInner)
                    let lambdaVal = Function(None, IdString "y", Expression(Arithmetic(Variable(IdString "y"), Add, Literal(Int 1))))
                    let inp = FuncApp(lambdaFn, lambdaVal)
                    Expect.equal (lambda (Ok inp)) expected  "(Lambda f. Lambda x. (f x)) Lambda y. y + 1 Failed"
                )
            testCase "lambda x. if true then x + 1 else x + 2" <| fun() ->
                (
                    let expected = Error (sprintf "No valid expression supplied for lambda calculation. Evaluated pure lambda is: %A" "x+1")
                    let iftrue = Expression (Arithmetic(Variable(IdString "x"), Add, Literal(Int 1)))
                    let orelse = Some (Expression (Arithmetic(Variable(IdString "x"), Add, Literal(Int 2))))
                    let conditional = Conditional(Literal(Bool true), iftrue, orelse)
                    let inp = Function(None, IdString "x", conditional)
                    Expect.equal (lambda (Ok inp)) expected "lambda x. if true then x + 1 else x + 2 failed"
                )

            // This test is expected to fail since variable inside conditional are declared as argument of Lambda function
            testProp "Lambda Condtional with Literal Comparison evaluates to Error message with Expression or Literal" <| (fun(Gen.GenCond exp) ->
            (   
                let expected = "No valid expression supplied for lambda calculation. Evaluated pure lambda is"
                let expected' = "Couldn't find a Variable in the environment"
                match lambda exp with
                | Ok (Expression (Literal lit)) -> Expect.isTrue true
                | Error msg when msg <> expected'-> Expect.isMatch (msg.Split ':' |> Seq.toList |> List.head) expected
                | Error msg'-> Expect.isMatch msg' expected'
                | _ -> 
                    Expect.isTrue false
            )
            )
            testProp "Literal Comparison Evaluates to Literal Bool" <| (fun (Gen.GenCompEx (exp)) ->
            (
                match lambda exp with
                | Ok (Expression(Literal(Bool b))) -> Expect.isTrue true
                | _ -> Expect.isTrue false
            )
            )
        ]

// main
[<EntryPoint>]
let main argv =
    let expectoconfig = {defaultConfig with verbosity = Logging.LogLevel.Debug}
 
    runTests expectoconfig Tests.topicTests |> ignore
    
       
    printfn "press any key to terminate"
    Console.ReadKey() |> ignore
    0 // return an integer exit code