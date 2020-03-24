module EndToEndTests

open Helpers
open Lexer
open Parser
open Combinator_runtime
open Expecto
open FsCheck
open System
open FsCheck


let fpy =
    tokeniser
        >> Parse
            >> Interpret

module Gen =
    let generateInt = 
        let length = System.Random().Next(1,10)
        let digitGen = Gen.elements(['0'..'9'])
        let arrToStr a = a |> Array.map Char.ToString |> String.concat "" 
        Gen.arrayOfLength length digitGen 
        |> Gen.map arrToStr 

    let generateDouble = 
        let length = System.Random().Next(1,10)
        let digitGen = Gen.elements(['0'..'9'])
        let dotGen = Gen.constant(".")
        let arrToStr a = a |> Array.map Char.ToString |> String.concat ""
        let numGen =  Gen.arrayOfLength length digitGen |> Gen.map arrToStr
        Gen.map3(fun num1 dot num2 ->  num1 + dot + num2) numGen dotGen numGen

    let generateNum = 
        Gen.oneof [generateInt; generateDouble]

    let generateString =
        let alphaGen = Gen.elements ([ 'a'..'z'] @ ['A'..'Z'])
        let digitGen = Gen.elements(['0'..'9'])
        let alphaNumGen = Gen.oneof [alphaGen ; digitGen]
        let arrToStr a = a |> Array.map Char.ToString |> String.concat "" 
        Gen.arrayOf alphaNumGen 
        |> Gen.map arrToStr 
            
    // string addition tests
    type Gen2Strings = Gen2Strings of string*string
    let GenString = 
        Gen.map2 (fun a b -> (a,b)) generateString generateString
        |> Arb.fromGen
        |> Arb.convert Gen2Strings (fun (Gen2Strings (a, b)) -> (a, b))

    type Gen2Nums = Gen2Nums of string*string
    let Gen2Nums = 
        Gen.map2 (fun a b -> (a,b)) generateNum generateNum
        |> Arb.fromGen
        |> Arb.convert Gen2Nums (fun (Gen2Nums (a, b)) -> (a, b))
    // arithmetic operations test


    let addToConfig config =
        let addedTypes = [
                typeof<Gen2Strings>.DeclaringType
                typeof<Gen2Nums>.DeclaringType
                
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
   
// Sample Unit End-to-End tests to check functionality
module Tests = 
    [<Tests>]
    let endToEndTestListWithExpecto =
        testList "End to End Tests with Expecto" [
            testProp "Arithmetics Eval" (fun (Gen.Gen2Nums (num1, num2)) ->
                    (   

                        let addition = "def f a b = a + b \n f " + num1 + " " + num2
                        let subtraction = "def f a b = a - b \n f " + num1 + " " +  num2
                        let mutiplication =  "def f a b = a * b \n f " + num1  + " " +  num2
                        let division = "def f a b = a / b \n f " + num1  + " " +  num2
                        let (|DOUBLEORINT|_|) = 
                            let innerFn x = 
                                match x with 
                                | Some(Literal (Double _))
                                | Some(Literal (Int _)) -> Some x
                                | _ -> None
                            innerFn
                                                
                        match (fpy addition, fpy subtraction, fpy mutiplication, fpy division) with
                        | (DOUBLEORINT _, DOUBLEORINT _, DOUBLEORINT _, DOUBLEORINT _ ) -> Expect.isTrue (true) 
                        | a, b, c, d ->  Expect.isOk (Error (sprintf "Integer or Double Output is expected, but %A, %A, %A, %A is given" a b c d))
                    )
                )

            testProp "String Addition" (fun (Gen.Gen2Strings (str1, str2)) ->
                    (   
                        let testfn = sprintf "def f a b = a + b \n f %A %A" str1 str2

                        match (fpy testfn) with
                        | Some(Literal (String _)) -> Expect.isTrue (true) 
                        | n ->  Expect.isOk (Error (sprintf "String Output is expected, but %A is given" n))
                    )
                )
            test "End to End Test 1" {
                let expected:Option<Ast> = Some (Literal (Int 7))
                Expect.equal ("def f x y = 2 * x + y \n f 2 3" |> tokeniser |> Parse |> Interpret ) expected "def f x y = 2 * x + y \n f 2 3"
            }

            test "End to End Test 2" {
                let expected:Option<Ast> = Some (Literal (Int 7))
                Expect.equal ("def f x y = (2 * x + y) \n f 2 3" |> tokeniser |> Parse |> Interpret ) expected "def f x y = (2 * x + y) \n f 2 3"
            }

            test "End to End Test 3" {
                let expected:Option<Ast> = Some (Literal (Int 10))
                Expect.equal ("def f x y = 2 * (x + y) \n f 2 3" |> tokeniser |> Parse |> Interpret ) expected "def f x y = 2 * (x + y) \n f 2 3"
            }

            test "End to End Test 4" {
                let expected:Option<Ast> = Some (Literal (Int 7))
                Expect.equal ("def f x y = (2 * x) + y \n f 2 3" |> tokeniser |> Parse |> Interpret ) expected "def f x y = (2 * x) + y \n f 2 3"
            }

            test "End to End Test 5" {
                let expected:Option<Ast> = Some (Literal (Int 7))
                Expect.equal ("def f x y = ((2 * x) + y) \n f 2 3" |> tokeniser |> Parse |> Interpret ) expected "def f x y = ((2 * x) + y) \n f 2 3"
            }

            test "End to End Test 6" {
                let expected:Option<Ast> = Some (Literal (Int 10))
                Expect.equal ("def f x y = (2 * ((x) + (y))) \n f 2 3" |> tokeniser |> Parse |> Interpret ) expected "def f x y = (2 * ((x) + (y))) \n f 2 3"
            }

            test "End to End Test 7" {
                let expected:Option<Ast> = Some (Literal (Int 0))
                Expect.equal ("def tempFtoC Ftemp = \n def p = Ftemp - 32 \n p \n tempFtoC 32" |> tokeniser |> Parse |> Interpret ) expected "def tempFtoC Ftemp = \n def p = Ftemp - 32 \n p \n tempFtoC 32"
            }

            //
            test "End to End Test 8" {
                let expected:Option<Ast> = Some (Literal (Int 2))
                Expect.equal ("def funkyListHead arr = \n Head arr \n funkyListHead [2,3,4]" |> tokeniser |> Parse |> Interpret ) expected "def funkyListHead arr = \n Head arr \n funkyListHead [2,3,4]"
            }

            test "End to End Test 9" {
                let expected:Option<Ast> = Some (Literal (Int 2))
                Expect.equal ("def funkyListHead arr = \n Head arr \n funkyListHead [2]" |> tokeniser |> Parse |> Interpret ) expected "def funkyListHead arr = \n Head arr \n funkyListHead [2]"
            }

            // printfn "%A" <| Interpret (Parse (tokeniser "def funkyIsList arr = \n isList arr \n funkyIsList [2,3]"))
            test "End to End Test 10" {
                let expected:Option<Ast> = Some (Literal (Bool true))
                Expect.equal ("def funkyIsList arr = \n isList arr \n funkyIsList [2,3]" |> tokeniser |> Parse |> Interpret ) expected "def funkyIsList arr = \n isList arr \n funkyIsList [2,3]"
            }

            // printfn "%A" <| Interpret (Parse (tokeniser "def funkyIsList arr = \n isList arr \n funkyIsList \"Tomputer Clarkitechture\""))
            test "End to End Test 11" {
                let expected:Option<Ast> = Some (Literal (Bool false))
                Expect.equal ("def funkyIsList arr = \n isList arr \n funkyIsList \"Tomputer Clarkitechture\"" |> tokeniser |> Parse |> Interpret ) expected "def funkyIsList arr = \n isList arr \n funkyIsList \"Tomputer Clarkitechture\""
            }

            // printfn "%A" <| Interpret (Parse (tokeniser "def funkyIsEmpty arr = \n isEmpty arr \n funkyIsEmpty [2,3]"))
            test "End to End Test 12" {
                let expected:Option<Ast> = Some (Literal (Bool false))
                Expect.equal ("def funkyIsEmpty arr = \n isEmpty arr \n funkyIsEmpty [2,3]" |> tokeniser |> Parse |> Interpret ) expected "def funkyIsEmpty arr = \n isEmpty arr \n funkyIsEmpty [2,3]"
            }

            // printfn "%A" <| Interpret (Parse (tokeniser "def funkyIsEmpty arr = \n isEmpty arr \n funkyIsEmpty []"))
            test "End to End Test 13" {
                let expected:Option<Ast> = Some (Literal (Bool true))
                Expect.equal ("def funkyIsEmpty arr = \n isEmpty arr \n funkyIsEmpty []" |> tokeniser |> Parse |> Interpret ) expected "def funkyIsEmpty arr = \n isEmpty arr \n funkyIsEmpty []"
            }
            
            // Interpret (Parse (tokeniser "def ifFuncTest x = \n if (x<1): true \n else: false \n ifFuncTest 3"))    
            test "End to End Test 14" {
                let expected:Option<Ast> = Some (Literal (Bool false))
                Expect.equal ("def ifFuncTest x = \n if (x<1): true \n else: false \n ifFuncTest 3" |> tokeniser |> Parse |> Interpret ) expected "def ifFuncTest x = \n if (x<1): true \n else: false \n ifFuncTest 3"
            }

            test "End to End Test 15" {
                let expected:Option<Ast> = Some (Literal (Int 4))
                Expect.equal ("def varDefinitionTest x = \n lst = [1,2,3] \n Head lst + x\n varDefinitionTest 3" |> tokeniser |> Parse |> Interpret ) expected "def varDefinitionTest x = \n lst = [1,2,3] \n Head lst + x\n varDefinitionTest 3"
            }

            test "End to End Test 16" {
                let expected:Option<Ast> = Some (Literal (Int 6))
                Expect.equal ("def varDefinitionTest x = \n lst = [1,2,3] \n 3*Head lst + x\n varDefinitionTest 3" |> tokeniser |> Parse |> Interpret ) expected "def varDefinitionTest x = \n lst = [1,2,3] \n 3*Head lst + x\n varDefinitionTest 3"
            }

            test "End to End Test 17" {
                let expected:Option<Ast> = Some (Literal (Int 20))
                Expect.equal ("def varDefinitionTest x = \n lst = [1,2,3] \n 3*Head lst + 2*(x+1)\n varDefinitionTest 3" |> tokeniser |> Parse |> Interpret ) expected "def varDefinitionTest x = \n lst = [1,2,3] \n 3*Head lst + 2*(x+1)\n varDefinitionTest 3"
            }
            
            test "End to End Test 18" {
                let expected:Option<Ast> = Some (Literal (Int 4))
                Expect.equal ("def varDefinitionTest x = \n lst = [1,2,3] \n (Head lst) + x\n varDefinitionTest 3" |> tokeniser |> Parse |> Interpret ) expected "def varDefinitionTest x = \n lst = [1,2,3] \n (Head lst) + x\n varDefinitionTest 3"
            }

            test "End to End Test 19" {
                let expected:Option<Ast> = Some (Literal (Bool false))
                Expect.equal ("def ifFuncTest x = \n if (x<1): true \n else: false \n ifFuncTest 3" |> tokeniser |> Parse |> Interpret ) expected "def ifFuncTest x = \n if (x<1): true \n else: false \n ifFuncTest 3"
            }

            test "End to End Test 20" {
                let expected:Option<Ast> = Some (Literal (Int 8))
                Expect.equal ("def varDefinitionTest x = \n y=5 \n x+y \n varDefinitionTest 3" |> tokeniser |> Parse |> Interpret ) expected "def varDefinitionTest x = \n y=5 \n x+y \n varDefinitionTest 3"
            }
        ]            

let endToEndTestsWithExpecto() =
    let expectoconfig = {defaultConfig with verbosity = Logging.LogLevel.Debug}

    runTests expectoconfig Tests.endToEndTestListWithExpecto |> ignore

