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
            Expect.equal ("def f x y = 2 * x + y \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret ) expected "def f x y = 2 * x + y \n f 2 3"
        }

        test "End to End Test 2" {
            let expected:Option<Ast> = Some (Literal (Int 7))
            Expect.equal ("def f x y = (2 * x + y) \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret ) expected "def f x y = (2 * x + y) \n f 2 3"
        }

        test "End to End Test 3" {
            let expected:Option<Ast> = Some (Literal (Int 10))
            Expect.equal ("def f x y = 2 * (x + y) \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret ) expected "def f x y = 2 * (x + y) \n f 2 3"
        }

        test "End to End Test 4" {
            let expected:Option<Ast> = Some (Literal (Int 7))
            Expect.equal ("def f x y = (2 * x) + y \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret ) expected "def f x y = (2 * x) + y \n f 2 3"
        }

        test "End to End Test 5" {
            let expected:Option<Ast> = Some (Literal (Int 7))
            Expect.equal ("def f x y = ((2 * x) + y) \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret ) expected "def f x y = ((2 * x) + y) \n f 2 3"
        }

        test "End to End Test 6" {
            let expected:Option<Ast> = Some (Literal (Int 10))
            Expect.equal ("def f x y = (2 * ((x) + (y))) \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret ) expected "def f x y = (2 * ((x) + (y))) \n f 2 3"
        }

        test "End to End Test 7" {
            let expected:Option<Ast> = Some (Literal (Int 0))
            Expect.equal ("def tempFtoC Ftemp = \n def p = Ftemp - 32 \n p \n tempFtoC 32" |> tokeniser |> pRun pExpr |> Interpret ) expected "def tempFtoC Ftemp = \n def p = Ftemp - 32 \n p \n tempFtoC 32"
        }
    ]

let endToEndTestsWithExpecto() =
    runTests defaultConfig endToEndTestListWithExpecto |> ignore