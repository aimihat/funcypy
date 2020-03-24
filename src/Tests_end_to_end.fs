module EndToEndTests

open Helpers
open Lexer
open Parser
open Combinator_runtime
open Expecto

[<Tests>]
// Sample Unit End-to-End tests to check functionality
let endToEndTestListWithExpecto =
    testList "End to End Tests with Expecto" [
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
    runTests defaultConfig endToEndTestListWithExpecto |> ignore