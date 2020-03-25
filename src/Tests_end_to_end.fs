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
            Expect.equal ("def f x y: 2 * x + y \n f 2 3" |> Tokenise |> Parse |> Interpret ) expected "def f x y: 2 * x + y \n f 2 3"
        }

        test "End to End Test 2" {
            let expected:Option<Ast> = Some (Literal (Int 7))
            Expect.equal ("def f x y: (2 * x + y) \n f 2 3" |> Tokenise |> Parse |> Interpret ) expected "def f x y: (2 * x + y) \n f 2 3"
        }

        test "End to End Test 3" {
            let expected:Option<Ast> = Some (Literal (Int 10))
            Expect.equal ("def f x y: 2 * (x + y) \n f 2 3" |> Tokenise |> Parse |> Interpret ) expected "def f x y: 2 * (x + y) \n f 2 3"
        }

        test "End to End Test 4" {
            let expected:Option<Ast> = Some (Literal (Int 7))
            Expect.equal ("def f x y: (2 * x) + y \n f 2 3" |> Tokenise |> Parse |> Interpret ) expected "def f x y: (2 * x) + y \n f 2 3"
        }

        test "End to End Test 5" {
            let expected:Option<Ast> = Some (Literal (Int 7))
            Expect.equal ("def f x y: ((2 * x) + y) \n f 2 3" |> Tokenise |> Parse |> Interpret ) expected "def f x y: ((2 * x) + y) \n f 2 3"
        }

        test "End to End Test 6" {
            let expected:Option<Ast> = Some (Literal (Int 10))
            Expect.equal ("def f x y: (2 * ((x) + (y))) \n f 2 3" |> Tokenise |> Parse |> Interpret ) expected "def f x y: (2 * ((x) + (y))) \n f 2 3"
        }

        test "End to End Test 7" {
            let expected:Option<Ast> = Some (Literal (Int 0))
            Expect.equal ("def tempFtoC Ftemp: \n def p: Ftemp - 32 \n p \n tempFtoC 32" |> Tokenise |> Parse |> Interpret ) expected "def tempFtoC Ftemp: \n def p: Ftemp - 32 \n p \n tempFtoC 32"
        }

        test "End to End Test 8" {
            let expected:Option<Ast> = Some (Literal (Int 2))
            Expect.equal ("def funkyListHead arr: \n head arr \n funkyListHead [2,3,4]" |> Tokenise |> Parse |> Interpret ) expected "def funkyListHead arr: \n head arr \n funkyListHead [2,3,4]"
        }

        test "End to End Test 9" {
            let expected:Option<Ast> = Some (Literal (Int 2))
            Expect.equal ("def funkyListHead arr: \n head arr \n funkyListHead [2]" |> Tokenise |> Parse |> Interpret ) expected "def funkyListHead arr = \n head arr \n funkyListHead [2]"
        }

        test "End to End Test 10" {
            let expected:Option<Ast> = Some (Literal (Bool true))
            Expect.equal ("def funkyIsList arr: \n is_list arr \n funkyIsList [2,3]" |> Tokenise |> Parse |> Interpret ) expected "def funkyIsList arr: \n is_list arr \n funkyIsList [2,3]"
        }

        test "End to End Test 11" {
            let expected:Option<Ast> = Some (Literal (Bool false))
            Expect.equal ("def funkyIsList arr: \n is_list arr \n funkyIsList \"funcypy is awesome\"" |> Tokenise |> Parse |> Interpret ) expected "def funkyIsList arr: \n is_list arr \n funkyIsList \"Tomputer Clarkitechture\""
        }

        test "End to End Test 12" {
            let expected:Option<Ast> = Some (Literal (Bool true))
            Expect.equal ("def funkyIsList arr: \n is_list arr \n funkyIsList [2,3]" |> Tokenise |> Parse |> Interpret ) expected "def funkyIsList arr: \n is_list arr \n funkyIsList [2,3]"
        }

        test "End to End Test 13" {
            let expected:Option<Ast> = Some (Literal (Bool true))
            Expect.equal ("def funkyIsList arr: \n is_list arr \n funkyIsList []" |> Tokenise |> Parse |> Interpret ) expected "def funkyIsList arr: \n is_list arr \n funkyis_list []"
        }
        
        test "End to End Test 14" {
            let expected:Option<Ast> = Some (Literal (Bool false))
            Expect.equal ("def ifFuncTest x: \n if (x<1): true \n else: false \n ifFuncTest 3" |> Tokenise |> Parse |> Interpret ) expected "def ifFuncTest x: \n if (x<1): true \n else: false \n ifFuncTest 3"
        }

        test "End to End Test 15" {
            let expected:Option<Ast> = Some (Literal (Int 4))
            Expect.equal ("def varDefinitionTest x: \n lst = [1,2,3] \n head lst + x\n varDefinitionTest 3" |> Tokenise |> Parse |> Interpret ) expected "def varDefinitionTest x: \n lst = [1,2,3] \n head lst + x\n varDefinitionTest 3"
        }

        test "End to End Test 16" {
            let expected:Option<Ast> = Some (Literal (Int 6))
            Expect.equal ("def varDefinitionTest x: \n lst = [1,2,3] \n 3*head lst + x\n varDefinitionTest 3" |> Tokenise |> Parse |> Interpret ) expected "def varDefinitionTest x: \n lst = [1,2,3] \n 3*head lst + x\n varDefinitionTest 3"
        }

        test "End to End Test 17" {
            let expected:Option<Ast> = Some (Literal (Int 20))
            Expect.equal ("def varDefinitionTest x: \n lst = [1,2,3] \n 3*head lst + 2*(x+1)\n varDefinitionTest 3" |> Tokenise |> Parse |> Interpret ) expected "def varDefinitionTest x: \n lst = [1,2,3] \n 3*head lst + 2*(x+1)\n varDefinitionTest 3"
        }
        
        test "End to End Test 18" {
            let expected:Option<Ast> = Some (Literal (Int 4))
            Expect.equal ("def varDefinitionTest x: \n lst = [1,2,3] \n (head lst) + x\n varDefinitionTest 3" |> Tokenise |> Parse |> Interpret ) expected "def varDefinitionTest x: \n lst = [1,2,3] \n (head lst) + x\n varDefinitionTest 3"
        }

        test "End to End Test 19" {
            let expected:Option<Ast> = Some (Literal (Bool false))
            Expect.equal ("def ifFuncTest x: \n if (x<1): true \n else: false \n ifFuncTest 3" |> Tokenise |> Parse |> Interpret ) expected "def ifFuncTest x: \n if (x<1): true \n else: false \n ifFuncTest 3"
        }

        test "End to End Test 20" {
            let expected:Option<Ast> = Some (Literal (Int 8))
            Expect.equal ("def varDefinitionTest x: \n y=5 \n x+y \n varDefinitionTest 3" |> Tokenise |> Parse |> Interpret ) expected "def varDefinitionTest x: \n y=5 \n x+y \n varDefinitionTest 3"
        }

        test "End to End Test 21: Subtraction test" {
            let expected:Option<Ast> = Some (Literal (Int -1))
            Expect.equal ("def f x y: x - y \n f 2 3" |> Tokenise |> Parse |> Interpret ) expected "def f x y: x - y \n f 2 3"
        }

        test "End to End Test 22: Addition test" {
            let expected:Option<Ast> = Some (Literal (Int 5))
            Expect.equal ("def f x y: x + y \n f 2 3" |> Tokenise |> Parse |> Interpret ) expected "def f x y: x - y \n f 2 3"
        }

        test "End to End Test 23: Addition and subtraction test" {
            let expected:Option<Ast> = Some (Literal (Int 4))
            Expect.equal ("def f x y: x + y - 1 \n f 2 3" |> Tokenise |> Parse |> Interpret ) expected "def f x y: x - y \n f 2 3"
        }

        test "End to End Test 24: Subtraction and Addition test" {
            let expected:Option<Ast> = Some (Literal (Int 0))
            Expect.equal ("def f x y: x - y + 1 \n f 2 3" |> Tokenise |> Parse |> Interpret ) expected "def f x y: x - y \n f 2 3"
        }

        test "End to End Test 25: Multiplication test" {
            let expected:Option<Ast> = Some (Literal (Int 6))
            Expect.equal ("def f x y: x * y \n f 2 3" |> Tokenise |> Parse |> Interpret ) expected "def f x y: x * y \n f 2 3"
        }

        test "End to End Test 26: Double Division test" {
            let expected:Option<Ast> = Some (Literal (Double 1.5))
            Expect.equal ("def f x y: x / y \n f 3.0 2.0" |> Tokenise |> Parse |> Interpret ) expected "def f x y: x / y \n f 3.0 2.0"
        }

        test "End to End Test 27: Int Division test" {
            let expected:Option<Ast> = Some (Literal (Int 5))
            Expect.equal ("def f x y: x / y \n f 10 2" |> Tokenise |> Parse |> Interpret ) expected "def f x y: x / y \n f 10 2"
        }

        test "End to End Test 28: Int and Double Division test 1" {
            let expected:Option<Ast> = Some (Literal (Double 5.0))
            Expect.equal ("def f x y: x / y \n f 10 2.0" |> Tokenise |> Parse |> Interpret ) expected "def f x y: x / y \n f 10 2.0"
        }

        test "End to End Test 29: Int and Double Division test 2" {
            let expected:Option<Ast> = Some (Literal (Double 4.0))
            Expect.equal ("def f x y: x / y \n f 10 2.5" |> Tokenise |> Parse |> Interpret ) expected "def f x y: x / y \n f 10 2.5"
        }

        test "End to End Test 30: Int and Double Multiplication test 1" {
            let expected:Option<Ast> = Some (Literal (Double 25.0))
            Expect.equal ("def f x y: x * y \n f 10 2.5" |> Tokenise |> Parse |> Interpret ) expected "def f x y: x * y \n f 10 2.5"
        }

        test "End to End Test 31: Int and Double Multiplication test 2" {
            let expected:Option<Ast> = Some (Literal (Double 27.5))
            Expect.equal ("def f x y: x * y \n f 11 2.5" |> Tokenise |> Parse |> Interpret ) expected "def f x y: x * y \n f 11 2.5"
        }

        test "End to End Test 32: Int and Double Multiplication and Addition" {
            let expected:Option<Ast> = Some (Literal (Double 28.5))
            Expect.equal ("def f x y: x * y + 1 \n f 11 2.5" |> Tokenise |> Parse |> Interpret ) expected "def f x y: x * y + 1 \n f 11 2.5"
        }

        test "End to End Test 33: Int and Double Multiplication and Subtraction with Brackets" {
            let expected:Option<Ast> = Some (Literal (Double 25.0))
            Expect.equal ("def f x y: (x - 1) * y \n f 11 2.5" |> Tokenise |> Parse |> Interpret ) expected "def f x y: (x - 1) * y \n f 11 2.5"
        }

        test "End to End Test 34: GE test in if statement" {
            let expected:Option<Ast> = Some (Literal (Bool true))
            Expect.equal ("def testGE x: \n if (x >= 5): true \n else: false \n testGE 6" |> Tokenise |> Parse |> Interpret ) expected "def testGE x: \n if (x >= 5): true \n else: false \n testGE 6"
        }

        test "End to End Test 35: LE test in if statement" {
            let expected:Option<Ast> = Some (Literal (Bool false))
            Expect.equal ("def testLE x: \n if (x <= 5): true \n else: false \n testLE 6" |> Tokenise |> Parse |> Interpret ) expected "def testLE x: \n if (x <= 5): true \n else: false \n testLE 6"
        }

        test "End to End Test 36: LE test in if statement on edge of LE" {
            let expected:Option<Ast> = Some (Literal (Bool true))
            Expect.equal ("def testGE x: \n if (x <= 5): true \n else: false \n testGE 5" |> Tokenise |> Parse |> Interpret ) expected "def testGE x: \n if (x <= 5): true \n else: false \n testGE 5"
        }

        test "End to End Test 37: LT test in if statement" {
            let expected:Option<Ast> = Some (Literal (Bool true))
            Expect.equal ("def testGE x: \n if (x>5): true \n else: false \n testGE 6" |> Tokenise |> Parse |> Interpret ) expected "def testGE x: \n if (x>5): true \n else: false \n testGE 6"
        }
    ]

let endToEndTestsWithExpecto() =
    runTests defaultConfig endToEndTestListWithExpecto |> ignore