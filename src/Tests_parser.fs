module ParserTests

open Helpers
open Parser
open Expecto
     
      
// 5 + 10
// let tokenInput1 = [TokLit (Int 5); TokBuiltInOp ADD; TokLit (Int 10)]
// let expParseOutput1 = Some(FuncApp(FuncApp (BuiltInFunc ADD, Const (Int 5)), Const (Int 10)), 3)

// // 5 * 10 + 3
// let tokenInput2 = [TokLit (Int 5); TokBuiltInOp MULTIPLY; TokLit (Int 10); TokBuiltInOp ADD; TokLit (Int 3)]
// let expParseOutput2 = Some(FuncApp(FuncApp(BuiltInFunc ADD, FuncApp (FuncApp (BuiltInFunc MULTIPLY, Const (Int 5)), Const (Int 10))), Const (Int 3)), 5)

// // 5
// let tokenInput3 = [TokLit (Int 5)]
// let expParseOutput3 = Some(Const (Int 5),1)

// // let f x = if(x <= 10) then x else x + 1
// let tokenInput4 = [TokSpecOp LET; TokIdentifier (IDString "f"); TokIdentifier (IDString "x"); TokSpecOp EQUALS; TokSpecOp IF; TokSpecOp LRB; TokIdentifier (IDString "x"); TokBuiltInOp LE; TokLit (Int 10); TokSpecOp RRB; TokSpecOp THEN; TokIdentifier (IDString "x"); TokSpecOp ELSE; TokIdentifier (IDString "x"); TokBuiltInOp ADD; TokLit (Int 1)] 
// let expParseOutput4 = Some(FuncDefExp(IDString "f",[Var (IDString "x")], Conditional(FuncApp (FuncApp (BuiltInFunc LE,Var (IDString "x")),Const (Int 10)), Var (IDString "x"), FuncApp (FuncApp (BuiltInFunc ADD, Var (IDString "x")), Const (Int 1)))), 16)

// // 5 + 10
// let tokenInput5 = [TokSpecOp IF; TokSpecOp LRB; TokLit (Int 6); TokBuiltInOp LE; TokLit (Int 10); TokSpecOp RRB; TokSpecOp THEN; TokLit (Int 5); TokSpecOp ELSE; TokLit (Int 6)]
// let expParseOutput5 = Some(Conditional(FuncApp(FuncApp (BuiltInFunc LE, Const (Int 6)), Const (Int 10)), Const (Int 5),Const (Int 6)), 10)
    
// // 5 +
// let tokenInput6 = [TokLit (Int 5); TokBuiltInOp ADD]
// let expParseOutput6 = Some (Const (Int 5), 1) // shouldnt this return an error

// // (6 < 10)   
// let tokenInput7 = [TokSpecOp LRB; TokLit (Int 6); TokBuiltInOp LE; TokLit (Int 10); TokSpecOp RRB]
// let expParseOutput7 = Some (FuncApp (FuncApp (BuiltInFunc LE,Const (Int 6)),Const (Int 10)), 5)   

// // (((6+10)))    
// let tokenInput8 = [TokSpecOp LRB; TokSpecOp LRB; TokSpecOp LRB; TokLit (Int 6); TokBuiltInOp ADD; TokLit (Int 10); TokSpecOp RRB; TokSpecOp RRB; TokSpecOp RRB]
// let expParseOutput8 = Some (FuncApp (FuncApp (BuiltInFunc ADD,Const (Int 6)),Const (Int 10)), 9)

// // lambda f -> 3 * 10 + f
// let tokenInput9 = [TokSpecOp LAMBDA; TokIdentifier (IDString "f"); TokSpecOp ARROWFUNC; TokLit (Int 3); TokBuiltInOp MULTIPLY; TokLit (Int 10); TokBuiltInOp ADD; TokIdentifier (IDString "f")]
// let expParseOutput9 = Some(Lambda(Var (IDString "f"), FuncApp(FuncApp(BuiltInFunc ADD, FuncApp(FuncApp (BuiltInFunc MULTIPLY, Const (Int 3)), Const (Int 10))), Var (IDString "f"))), 8)

[<Tests>]
// Sample Unit tests for parseT3 to check functionality
let parserTestListWithExpecto =
    testList "Test Group for Parser with Expecto" [
        test "Parser Test 1" {
            let tokenInput1 =  [TokSpecOp DEF ; TokIdentifier ("x") ; TokSpecOp EQUALS ; TokWhitespace LineFeed ; TokLit (Int 2) ; TokWhitespace LineFeed ; TokIdentifier ("x")]
            let expected = Some(FuncDefExp("x", Literal (Int 2), Variable "x"), 7)
            Expect.equal (pRun pExpr tokenInput1) expected "Parsing inp: def x = \n 2 \n x"     
        }

        test "Bad Function Definition without new line" {
            let tokenInput = [TokSpecOp DEF; TokIdentifier "x"; TokWhitespace LineFeed ; TokSpecOp EQUALS; TokLit (Int 2)]
            let expected = None
            Expect.equal (pRun pExpr tokenInput) expected "Parsing inp: def x = 2" 
        }
        
        test "Bad Function Definition with new line" {
            let tokenInput = [TokSpecOp DEF; TokIdentifier "x"; TokSpecOp EQUALS; TokLit (Int 2)]
            let expected = None
            Expect.equal (pRun pExpr tokenInput) expected "Parsing inp: def x = \n 2" 
        }
        
        test "Simple Nested Function Definition" {
            let tokenInput = [TokSpecOp DEF; TokIdentifier "f"; TokIdentifier "y"; TokSpecOp EQUALS; TokWhitespace LineFeed; TokSpecOp DEF; TokIdentifier "x"; TokSpecOp EQUALS; TokLit (Int 2); TokWhitespace LineFeed; TokIdentifier "x"; TokBuiltInOp (Arithm Multiply); TokIdentifier "y"; TokWhitespace LineFeed; TokIdentifier "f"; TokLit (Int 2)]
            let expected = Some (FuncDefExp("f", Lambda("y", FuncDefExp("x",Literal (Int 2), Call(Call (BuiltInFunc (Arithm Multiply),Variable "x",ID 0), Variable "y",ID 0))),Call (Variable "f",Literal (Int 2),ID 0)), 16)
            Expect.equal (pRun pExpr tokenInput) expected "Parsing inp: def f y = \n def x = 2 \n x * y \n f 2" 
        }
        
        test "Function Definition with 2 inputs" {
            let tokenInput = [TokSpecOp DEF ; TokIdentifier "f" ; TokIdentifier "x" ; TokIdentifier "y" ; TokSpecOp EQUALS; TokLit (Int 2); TokBuiltInOp (Arithm Multiply) ; TokIdentifier "x"; TokWhitespace LineFeed; TokIdentifier "f"; TokLit (Int 2) ; TokLit (Int 3)]
            let expected = Some(FuncDefExp("f",Lambda("x",Lambda("y",Call(Call (BuiltInFunc (Arithm Multiply),Literal (Int 2),ID 0), Variable "x",ID 0))), Call (Call (Variable "f",Literal (Int 2),ID 0),Literal (Int 3),ID 0)), 12)
            Expect.equal (pRun pExpr tokenInput) expected "Parsing inp: def f y = \n def x = 2 \n x * y \n f 2" 
        }

        test "Function Definition Test 1" {
            let tokenInput = [TokSpecOp DEF ; TokIdentifier "f" ; TokIdentifier "x" ; TokSpecOp EQUALS ; TokWhitespace LineFeed; TokLit (Int 2); TokBuiltInOp (Arithm Multiply) ; TokIdentifier "x"; TokWhitespace LineFeed; TokIdentifier "f"; TokLit (Int 2)]
            let expected = Some(FuncDefExp("f",Lambda("x",Call(Call (BuiltInFunc (Arithm Multiply),Literal (Int 2),ID 0), Variable "x",ID 0)),Call (Variable "f",Literal (Int 2),ID 0)), 11)
            Expect.equal (pRun pExpr tokenInput) expected "Parsing inp: def f x = \n 2*x \n f 2" 
        }

        test "Function Definition containing Lambda" {
            let tokenInput = [TokSpecOp DEF ; TokIdentifier "f" ; TokSpecOp EQUALS ; TokSpecOp LAMBDA ; TokIdentifier "x" ; TokSpecOp ARROWFUNC ; TokIdentifier "x" ; TokBuiltInOp (Arithm Multiply) ; TokLit (Int 2) ; TokWhitespace LineFeed ; TokIdentifier "f"; TokLit (Int 2)]
            let expected = Some(FuncDefExp("f", Lambda("x", Call(Call (BuiltInFunc (Arithm Multiply),Variable "x",ID 0), Literal (Int 2),ID 0)),Call (Variable "f",Literal (Int 2),ID 0)), 12)
            Expect.equal (pRun pExpr tokenInput) expected "Parsing inp: def f = lambda x -> x*2 \n f 2" 
        }

        test "2 Function Definitions then one return" {
            let tokenInput = [TokSpecOp DEF; TokIdentifier "x"; TokSpecOp EQUALS; TokLit (Int 2) ; TokWhitespace LineFeed; TokSpecOp DEF; TokIdentifier "y"; TokSpecOp EQUALS ; TokLit (Int 2); TokWhitespace LineFeed; TokIdentifier "x"]
            let expected = Some(FuncDefExp("x",Literal (Int 2),FuncDefExp ("y",Literal (Int 2),Variable "x")), 11)
            Expect.equal (pRun pExpr tokenInput) expected "Parsing inp: def x = 2 \n def y = 2 \n x" 
        }

        // test "Test Name Here" {
        //     let tokenInput =  [TokSpecOp DEF; TokIdentifier "f"; TokSpecOp EQUALS; TokSpecOp LAMBDA; TokIdentifier "x"; TokSpecOp ARROWFUNC; TokIdentifier "x"; TokBuiltInOp (Arithm Multiply); TokLit (Int 2); TokWhitespace LineFeed; TokIdentifier "f"; TokLit (Int 2)]
        //     let expected = Some(FuncDefExp("x", Literal (Int 2), FuncDefExp), 12)
        //     Expect.equal (pRun pExpr tokenInput) expected "Parsing inp: def x = 2 '\n def y = 2 \n x"     
        // }

        // test "Parser Test Lambda" {
        //     let tokenInput = [TokSpecOp LAMBDA; TokIdentifier "f"; TokSpecOp ARROWFUNC; TokLit (Int 3); TokBuiltInOp MULTIPLY; TokLit (Int 10); TokBuiltInOp ADD; TokIdentifier (IDString "f")]
        //     let expected = Some(Lambda(Lit (IDString "f"), FuncApp(FuncApp(BuiltInFunc ADD, FuncApp(FuncApp (BuiltInFunc MULTIPLY, Const (Int 3)), Const (Int 10))), Var (IDString "f"))), 8)
        //     Expect.equal (pRun pExpr tokenInput1) expected "Parsing inp: lambda f -> 3 * 10 + f"
        // }

        // test "Parser Test 2" {
        //     let expected = expParseOutput2
        //     Expect.equal (pRun pExpr tokenInput2) expected "Parsing inp: 5 * 10 + 3"
        // }

        // test "Parser Test 3" {
        //     let expected = expParseOutput3
        //     Expect.equal (pRun pExpr tokenInput3) expected "Parsing inp: 5"
        // }

        // test "Parser Test 4" {
        //     let expected = expParseOutput4
        //     Expect.equal (pRun pExpr tokenInput4) expected "Parsing inp: let f x = if(x <= 10) then x else x + 1"
        // }

        // test "Parser Test 5" {
        //     let expected = expParseOutput5
        //     Expect.equal (pRun pExpr tokenInput5) expected "Parsing inp: 5 + 10"
        // }

        // test "Parser Test 6" {
        //     let expected = expParseOutput6
        //     Expect.equal (pRun pExpr tokenInput6) expected "Parsing inp: 5 +"
        // }

        // test "Parser Test 7" {
        //     let expected = expParseOutput7
        //     Expect.equal (pRun pExpr tokenInput7) expected "Parsing inp: (6 < 10)"
        // }

        // test "Parser Test 8" {
        //     let expected = expParseOutput8
        //     Expect.equal (pRun pExpr tokenInput8) expected "Parsing inp: (((6+10)))"
        // }

        test "Pair test 1: Empty Pair " {
            let emptyPair = [TokSpecOp LSB ; TokSpecOp RSB]
            let expected = Some (Pair (Null,Null,ID 0), 2)
            Expect.equal (pRun pExpr emptyPair) expected "Parsing inp: []"
        }

        test "Pair parse test 2: Single Pair" {
            let singlePair = [TokSpecOp LSB ; TokLit (Int 1) ; TokSpecOp RSB]
            let expected = Some (Pair (Literal (Int 1),Null,ID 0), 3)
            Expect.equal (pRun pExpr singlePair) expected "Parsing inp: [1]"
        }

        test "Pair parse test 3: Two Pair" {
            let twoPair = [TokSpecOp LSB ; TokLit (Int 5) ; TokSpecOp COMMA ; TokLit (Int 6) ; TokSpecOp RSB]
            let expected = Some(Pair(Pair(Literal (Int 5), Literal (Int 6),ID 0), Null, ID 0), 5)
            Expect.equal (pRun pExpr twoPair) expected "Parsing inp: [5,6]"
        }

        test "Pair parse test 4: Three Pair" {
            let threePair = [TokSpecOp LSB ; TokLit (Int 2) ; TokSpecOp COMMA ; TokLit (Int 3) ; TokSpecOp COMMA ; TokLit (Int 4) ; TokSpecOp RSB]
            let expected = Some (Pair(Pair(Pair(Literal (Int 2), Literal (Int 3), ID 0), Literal (Int 4), ID 0), Null, ID 0), 7)
            Expect.equal (pRun pExpr threePair) expected "Parsing inp: [2,3,4]"
        }

        test "Pair parse test 4: Four Pair" {
            let threePair = [TokSpecOp LSB ; TokLit (Int 2) ; TokSpecOp COMMA ; TokLit (Int 3) ; TokSpecOp COMMA ; TokLit (Int 4) ; TokSpecOp COMMA ; TokLit (Int 5) ;TokSpecOp RSB]
            let expected = Some (Pair(Pair(Pair(Pair(Literal (Int 2), Literal (Int 3), ID 0), Literal (Int 4), ID 0), Literal (Int 5), ID 0), Null, ID 0), 9)
            Expect.equal (pRun pExpr threePair) expected "Parsing inp: [2,3,4,5]"
        }
    ]

let parserTestsWithExpecto() =
    runTests defaultConfig parserTestListWithExpecto |> ignore