module ParserTests

open Helpers
open Parser
open Expecto
open Lexer

[<Tests>]
// Sample Unit tests for parseT3 to check functionality
let parserTestListWithExpecto =
    testList "Test Group for Parser with Expecto" [
        test "Parser Test 1" {
            let tokenInput1 =  [TokSpecOp DEF ; TokIdentifier ("x") ; TokSpecOp COLON ; TokWhitespace LineFeed ; TokLit (Int 2) ; TokWhitespace LineFeed ; TokIdentifier ("x")]
            let expected = Some(FuncDefExp("x", Literal (Int 2), Variable "x"), 7)
            Expect.equal (pRun pAst tokenInput1) expected "Parsing inp: def x: \n 2 \n x"     
        }

        test "Bad Function Definition without new line" {
            let tokenInput = [TokSpecOp DEF; TokIdentifier "x"; TokWhitespace LineFeed ; TokSpecOp EQUALS; TokLit (Int 2)]
            // let expected = None
            Expect.throws (fun _ -> pRun pAst tokenInput |> ignore) "Parsing inp: def x = 2" 
        }
                
        test "Bad Function Definition with missing equals" {
            let tokenInput = [TokSpecOp DEF; TokIdentifier "x"; TokLit (Int 2)]
            // let expected = failwith "Missing TokSpecOp EQUALS"
            Expect.throws (fun _ -> pRun pAst tokenInput |> ignore) "Parsing inp: def x = \n 2" 
        }

        test "Bad Function Definition with missing equals and newline" {
            let tokenInput = [TokSpecOp DEF; TokIdentifier "x"; TokWhitespace LineFeed; TokLit (Int 2)]
            // let expected = failwith "Missing TokSpecOp EQUALS"
            Expect.throws (fun _ -> pRun pAst tokenInput |> ignore) "Parsing inp: def x \n 2" 
        }

        test "Bad Function Definition with brackets around body" {
            let tokenInput = [TokSpecOp DEF; TokIdentifier "x"; TokSpecOp LRB ; TokLit (Int 2) ; TokSpecOp RRB]
            // let expected = failwith "Missing TokSpecOp EQUALS"
            Expect.throws (fun _ -> pRun pAst tokenInput |> ignore) "Parsing inp: def x \n 2" 
        }

        test "Uneven number of brackets around value 2" {
            let tokenInput = [TokSpecOp LRB ; TokSpecOp LRB ; TokLit (Int 5) ;TokSpecOp RRB]
            let expected = None
            Expect.equal (pRun pAst tokenInput) expected "Parsing inp: ((5)" 
        }

        test "Bad Lambda Definition with new line" {
            let tokenInput = [TokSpecOp DEF ; TokIdentifier "f" ; TokSpecOp COLON ; TokSpecOp LAMBDA ; TokIdentifier "x" ; TokIdentifier "x" ; TokBuiltInOp (Arithm Multiply) ; TokLit (Int 2) ; TokWhitespace LineFeed ; TokIdentifier "f"; TokLit (Int 2)]
            // let expected = failwithf "Missing TokSpecOp ARROWFUNC"
            Expect.throws (fun _ -> pRun pAst tokenInput |> ignore) "Parsing inp: Bad Lambda definition" 
        }
 
        test "Simple Nested Function Definition" {
            let tokenInput = [TokSpecOp DEF; TokIdentifier "f"; TokIdentifier "y"; TokSpecOp COLON; TokWhitespace LineFeed; TokSpecOp DEF; TokIdentifier "x"; TokSpecOp COLON; TokLit (Int 2); TokWhitespace LineFeed; TokIdentifier "x"; TokBuiltInOp (Arithm Multiply); TokIdentifier "y"; TokWhitespace LineFeed; TokIdentifier "f"; TokLit (Int 2)]
            let expected = Some (FuncDefExp("f", Lambda("y", FuncDefExp("x",Literal (Int 2), Call(Call (BuiltInFunc (Arithm Multiply),Variable "x",ID 0), Variable "y",ID 0))),Call (Variable "f",Literal (Int 2),ID 0)), 16)
            Expect.equal (pRun pAst tokenInput) expected "Parsing inp: def f y: \n def x: 2 \n x * y \n f 2" 
        }
        
        test "Function Definition with 2 inputs" {
            let tokenInput = [TokSpecOp DEF ; TokIdentifier "f" ; TokIdentifier "x" ; TokIdentifier "y" ; TokSpecOp COLON; TokLit (Int 2); TokBuiltInOp (Arithm Multiply) ; TokIdentifier "x"; TokWhitespace LineFeed; TokIdentifier "f"; TokLit (Int 2) ; TokLit (Int 3)]
            let expected = Some(FuncDefExp("f",Lambda("x",Lambda("y",Call(Call (BuiltInFunc (Arithm Multiply),Literal (Int 2),ID 0), Variable "x",ID 0))), Call (Call (Variable "f",Literal (Int 2),ID 0),Literal (Int 3),ID 0)), 12)
            Expect.equal (pRun pAst tokenInput) expected "Parsing inp: def f x y: \n def x = 2*x \n f 2 3" 
        }

        test "Function Definition Test 1" {
            let tokenInput = [TokSpecOp DEF ; TokIdentifier "f" ; TokIdentifier "x" ; TokSpecOp COLON ; TokWhitespace LineFeed; TokLit (Int 2); TokBuiltInOp (Arithm Multiply) ; TokIdentifier "x"; TokWhitespace LineFeed; TokIdentifier "f"; TokLit (Int 2)]
            let expected = Some(FuncDefExp("f",Lambda("x",Call(Call (BuiltInFunc (Arithm Multiply),Literal (Int 2),ID 0), Variable "x",ID 0)),Call (Variable "f",Literal (Int 2),ID 0)), 11)
            Expect.equal (pRun pAst tokenInput) expected "Parsing inp: def f x = \n 2*x \n f 2" 
        }

        test "Function Definition containing Lambda" {
            let tokenInput = [TokSpecOp DEF ; TokIdentifier "f" ; TokSpecOp COLON ; TokSpecOp LAMBDA ; TokIdentifier "x" ; TokSpecOp COLON ; TokIdentifier "x" ; TokBuiltInOp (Arithm Multiply) ; TokLit (Int 2) ; TokWhitespace LineFeed ; TokIdentifier "f"; TokLit (Int 2)]
            let expected = Some(FuncDefExp("f", Lambda("x", Call(Call (BuiltInFunc (Arithm Multiply),Variable "x",ID 0), Literal (Int 2),ID 0)),Call (Variable "f",Literal (Int 2),ID 0)), 12)
            Expect.equal (pRun pAst tokenInput) expected "Parsing inp: def f: lambda x: x*2 \n f 2" 
        }

        test "2 Function Definitions then one return" {
            let tokenInput = [TokSpecOp DEF; TokIdentifier "x"; TokSpecOp COLON; TokLit (Int 2) ; TokWhitespace LineFeed; TokSpecOp DEF; TokIdentifier "y"; TokSpecOp COLON ; TokLit (Int 2); TokWhitespace LineFeed; TokIdentifier "x"]
            let expected = Some(FuncDefExp("x",Literal (Int 2),FuncDefExp ("y",Literal (Int 2),Variable "x")), 11)
            Expect.equal (pRun pAst tokenInput) expected "Parsing inp: def x: 2 \n def y: 2 \n x" 
        }

        test "Pair test 1: Empty Pair " {
            let emptyPair = [TokSpecOp LSB ; TokSpecOp RSB]
            let expected = Some (Pair (Null,Null,ID 0), 2)
            Expect.equal (pRun pAst emptyPair) expected "Parsing inp: []"
        }

        test "Pair parse test 2: Single Pair" {
            let singlePair = [TokSpecOp LSB ; TokLit (Int 1) ; TokSpecOp RSB]
            let expected = Some (Pair (Literal (Int 1),Null,ID 0), 3)
            Expect.equal (pRun pAst singlePair) expected "Parsing inp: [1]"
        }

        test "Pair parse test 3: Two Pair" {
            let twoPair = [TokSpecOp LSB ; TokLit (Int 5) ; TokSpecOp COMMA ; TokLit (Int 6) ; TokSpecOp RSB]
            let expected = Some(Pair(Literal (Int 5), Pair(Literal (Int 6), Null, ID 0), ID 0), 5)
            Expect.equal (pRun pAst twoPair) expected "Parsing inp: [5,6]"
        }

        test "Pair parse test 4: Three Pair" {
            let threePair = [TokSpecOp LSB ; TokLit (Int 2) ; TokSpecOp COMMA ; TokLit (Int 3) ; TokSpecOp COMMA ; TokLit (Int 4) ; TokSpecOp RSB]
            let expected = Some (Pair(Literal (Int 2), Pair(Literal (Int 3), Pair(Literal (Int 4), Null, ID 0), ID 0), ID 0), 7)
            Expect.equal (pRun pAst threePair) expected "Parsing inp: [2,3,4]"
        }

        test "Pair parse test 4: Four Pair" {
            let threePair = [TokSpecOp LSB ; TokLit (Int 2) ; TokSpecOp COMMA ; TokLit (Int 3) ; TokSpecOp COMMA ; TokLit (Int 4) ; TokSpecOp COMMA ; TokLit (Int 5) ;TokSpecOp RSB]
            let expected = Some (Pair(Literal (Int 2), Pair(Literal (Int 3),Pair (Literal (Int 4),Pair (Literal (Int 5),Null,ID 0),ID 0),ID 0),ID 0), 9)
            Expect.equal (pRun pAst threePair) expected "Parsing inp: [2,3,4,5]"
        }

        
        test "Parser and Lexer Test 1" {
            let expected = Some(FuncDefExp("funkyListHead",Lambda ("arr",Call (BuiltInFunc (ListF Head),Variable "arr",ID 0)),Call (Variable "funkyListHead",Pair (Null,Null,ID 0),ID 0)), 11)
            Expect.equal ("def funkyListHead arr: \n head arr \n funkyListHead []" |> Tokenise |> pRun pAst) expected "def funkyListHead arr: \n head arr \n funkyListHead []"
        }

        test "Parser and Lexer Test 2" {
            let expected = Some(FuncDefExp("funkyListTail",Lambda ("arr",Call (BuiltInFunc (ListF Tail),Variable "arr",ID 0)),Call(Variable "funkyListTail",Pair (Literal (Int 2),Pair (Literal (Int 3),Null,ID 0),ID 0),ID 0)), 14)
            Expect.equal ("def funkyListTail arr: \n tail arr \n funkyListTail [2,3]" |> Tokenise |> pRun pAst) expected "def funkyListTail arr: \n tail arr \n funkyListTail [2,3]"
        }    
        
        test "Parser and Lexer Test 3" {
            let expected = Some(FuncDefExp("funkyListTail",Lambda ("arr",Call (BuiltInFunc (ListF Tail),Variable "arr",ID 0)),Call(Variable "funkyListTail",Pair(Literal (Int 2),Pair(Literal (Int 3),Pair(Literal (Int 4),Pair (Literal (Int 5),Pair (Literal (Int 6),Null,ID 0),ID 0),ID 0),ID 0),ID 0),ID 0)), 20)
            Expect.equal ("def funkyListTail arr: \n tail arr \n funkyListTail [2,3,4,5,6]" |> Tokenise |> pRun pAst) expected "def funkyListTail arr: \n tail arr \n funkyListTail [2,3,4,5,6]"
        }
    ]

let parserTestsWithExpecto() =
    runTests defaultConfig parserTestListWithExpecto |> ignore