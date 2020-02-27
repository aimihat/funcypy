module Testing

open Common
open Parser
open Expecto
// open FSCheck
    
// pRun pExpr inp7 |> printf "%A"

// 5 + 10
let tokenInput1 = [TokLit (Int 5); TokBuiltInOp ADD; TokLit (Int 10)]
let expParseOutput1 = Some(FuncApp(FuncApp (BuiltInFunc ADD, Const (Int 5)), Const (Int 10)), 3)

// 5 * 10 + 3
let tokenInput2 = [TokLit (Int 5); TokBuiltInOp MULTIPLY; TokLit (Int 10); TokBuiltInOp ADD; TokLit (Int 3)]
let expParseOutput2 = Some(FuncApp(FuncApp(BuiltInFunc ADD, FuncApp (FuncApp (BuiltInFunc MULTIPLY, Const (Int 5)), Const (Int 10))), Const (Int 3)), 5)

// 5
let tokenInput3 = [TokLit (Int 5)]
let expParseOutput3 = Some(Const (Int 5),1) // null

// let f x = if(x <= 10) then x else x + 1
let tokenInput4 = [TokSpecOp LET; TokWhitespace (Space); TokIdentifier (IDString "f"); TokWhitespace (Space); TokIdentifier (IDString "x"); TokSpecOp EQUALS; TokWhitespace (Space);TokSpecOp IF; TokSpecOp LRB; TokIdentifier (IDString "x"); TokBuiltInOp LE; TokLit (Int 10); TokSpecOp RRB; TokSpecOp THEN; TokIdentifier (IDString "x"); TokSpecOp ELSE; TokIdentifier (IDString "x"); TokBuiltInOp ADD; TokLit (Int 1)] 
let expParseOutput4 = Some(FuncDefExp(IDString "f",[Var (IDString "x")], Conditional(FuncApp (FuncApp (BuiltInFunc LE,Var (IDString "x")),Const (Int 10)), Var (IDString "x"), FuncApp (FuncApp (BuiltInFunc ADD, Var (IDString "x")), Const (Int 1)))), 19)

// 5 + 10
let tokenInput5 = [TokSpecOp IF; TokSpecOp LRB; TokLit (Int 6); TokBuiltInOp LE; TokLit (Int 10); TokSpecOp RRB; TokSpecOp THEN; TokLit (Int 5); TokSpecOp ELSE; TokLit (Int 6)]
let expParseOutput5 = Some(Conditional(FuncApp(FuncApp (BuiltInFunc LE, Const (Int 6)), Const (Int 10)), Const (Int 5),Const (Int 6)), 10)
    
// 5 +
let tokenInput6 = [TokLit (Int 5); TokBuiltInOp ADD]
let expParseOutput6 = Some (Const (Int 5), 1) // shouldnt this return an error

// (6 < 10)   
let tokenInput7 = [TokSpecOp LRB; TokLit (Int 6); TokBuiltInOp LE; TokLit (Int 10); TokSpecOp RRB]
let expParseOutput7 = Some (FuncApp (FuncApp (BuiltInFunc LE,Const (Int 6)),Const (Int 10)), 5)   

// (((6+10)))    
let tokenInput8 = [TokSpecOp LRB; TokSpecOp LRB; TokSpecOp LRB; TokLit (Int 6); TokBuiltInOp ADD; TokLit (Int 10); TokSpecOp RRB; TokSpecOp RRB; TokSpecOp RRB]
let expParseOutput8 = Some (FuncApp (FuncApp (BuiltInFunc ADD,Const (Int 6)),Const (Int 10)), 9)

// lambda f -> 3 * 10 + f
let tokenInput9 = [TokSpecOp LAMBDA; TokIdentifier (IDString "f"); TokSpecOp ARROWFUNC; TokLit (Int 3); TokBuiltInOp MULTIPLY; TokLit (Int 10); TokBuiltInOp ADD; TokIdentifier (IDString "f")]
let expParseOutput9 = Some(Lambda(Var (IDString "f"), FuncApp(FuncApp(BuiltInFunc ADD, FuncApp(FuncApp (BuiltInFunc MULTIPLY, Const (Int 3)), Const (Int 10))), Var (IDString "f"))), 8)

// This function is not currently being used
let makeTokenListIntoTest (testName:string, tokInp:List<Token>, astResult:Option<Ast * int>) =        
    test testName {
        let actual = (pRun pExpr tokInp)
        let expected = astResult
        Expect.equal actual expected "Message goes here"
    }

[<Tests>]
// Sample Unit tests for parseT3 to check functionality
let parserTestListWithExpecto =
    testList "Test Group for Parser with Expecto" [
        test "Parser Test 1" {
            let expected = expParseOutput1
            Expect.equal (pRun pExpr tokenInput1) expected "Parsing inp: 5 + 10"     
        }

        test "Parser Test 2" {
            let expected = expParseOutput2
            Expect.equal (pRun pExpr tokenInput2) expected "Parsing inp: 5 * 10 + 3"
        }

        test "Parser Test 3" {
            let expected = expParseOutput3
            Expect.equal (pRun pExpr tokenInput3) expected "Parsing inp: 5"
        }

        test "Parser Test 4" {
            let expected = expParseOutput4
            Expect.equal (pRun pExpr tokenInput4) expected "Parsing inp: let f x = if(x <= 10) then x else x + 1"
        }

        test "Parser Test 5" {
            let expected = expParseOutput5
            Expect.equal (pRun pExpr tokenInput5) expected "Parsing inp: 5 + 10"
        }

        test "Parser Test 6" {
            let expected = expParseOutput6
            Expect.equal (pRun pExpr tokenInput6) expected "Parsing inp: 5 +"
        }

        test "Parser Test 7" {
            let expected = expParseOutput7
            Expect.equal (pRun pExpr tokenInput7) expected "Parsing inp: (6 < 10)"
        }

        test "Parser Test 8" {
            let expected = expParseOutput8
            Expect.equal (pRun pExpr tokenInput8) expected "Parsing inp: (((6+10)))"
        }

        test "Parser Test 9" {
            let expected = expParseOutput9
            Expect.equal (pRun pExpr tokenInput9) expected "Parsing inp: lambda f -> 3 * 10 + f"
        }

        // test "Parser Test 10" {
        //     let expected = expParseOutput10
        //     Expect.equal (pRun pExpr tokenInput10) expected "Parsing inp: 9"
        // }
    ]

let parserTestsWithExpecto() =
    runTests defaultConfig parserTestListWithExpecto |> ignore