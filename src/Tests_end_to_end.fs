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
            Expect.equal ("def f x y = 2 * x + y \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret ) expected "string with useful information on test"     
        }

        test "End to End Test 2" {
            let expected:Option<Ast> = Some (Literal (Int 7))
            Expect.equal ("def f x y = (2 * x + y) \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret ) expected "string with useful information on test"     
        }

        test "End to End Test 3" {
            let expected:Option<Ast> = Some (Literal (Int 10))
            Expect.equal ("def f x y = 2 * (x + y) \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret ) expected "string with useful information on test"     
        }

        test "End to End Test 4" {
            let expected:Option<Ast> = Some (Literal (Int 7))
            Expect.equal ("def f x y = (2 * x) + y \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret ) expected "string with useful information on test"     
        }

        test "End to End Test 5" {
            let expected:Option<Ast> = Some (Literal (Int 7))
            Expect.equal ("def f x y = ((2 * x) + y) \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret ) expected "string with useful information on test"     
        }

        test "End to End Test 6" {
            let expected:Option<Ast> = Some (Literal (Int 10))
            Expect.equal ("def f x y = (2 * ((x) + (y))) \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret ) expected "string with useful information on test"     
        }

        // test "End to End Test 7" {
        //     let stringInput1 = ""
        //     let expected:Option<Ast> = Some (Literal (Int 7))
        //     Expect.equal (stringInput1 |> tokeniser |> pRun pExpr |> Interpret ) expected "string with useful information on test"     
        // }

        // test "End to End Test 8" {
        //     let stringInput1 = ""
        //     let expected:Option<Ast> = Some (Literal (Int 7))
        //     Expect.equal (stringInput1 |> tokeniser |> pRun pExpr |> Interpret ) expected "string with useful information on test"     
        // }

        // test "End to End Test 9" {
        //     let stringInput1 = ""
        //     let expected:Option<Ast> = Some (Literal (Int 7))
        //     Expect.equal (stringInput1 |> tokeniser |> pRun pExpr |> Interpret ) expected "string with useful information on test"     
        // }

        // test "End to End Test 10" {
        //     let stringInput1 = ""
        //     let expected:Option<Ast> = Some (Literal (Int 7))
        //     Expect.equal (stringInput1 |> tokeniser |> pRun pExpr |> Interpret ) expected "string with useful information on test"     
        // }
    ]

let endToEndTestsWithExpecto() =
    runTests defaultConfig endToEndTestListWithExpecto |> ignore