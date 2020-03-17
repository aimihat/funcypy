module Testslexer

open Expecto
open Helpers
open Lexer

[<Tests>]
let Test1 =
 testCase "Integer Test" <| fun () ->
   let expected = [TokLit (Int 1)]
   Expect.equal (tokeniser "1") expected "Tokenise an integer"

[<Tests>]
let Test2 =
 testCase "Whitespaces" <| fun () ->
   let expected = [TokWhitespace Space; TokWhitespace Space; TokWhitespace Space; TokWhitespace Space; TokWhitespace Space]
   Expect.equal (tokeniser "     ") expected "Tokenise whitespaces"

[<Tests>]
let Test3 =
 testCase "Equation test true" <| fun () ->
   let expected = [TokLit (Int 1); TokBuiltInOp (Arithm Add); TokLit (Int 1); TokSpecOp EQUALS; TokLit (Int 2)]
   Expect.equal (tokeniser "1+1=2") expected "Tokenise a valid equation"

[<Tests>]
let Test4 =
 testCase "Equation test false" <| fun () ->
   let expected = [TokLit (Int 1); TokBuiltInOp (Arithm Add); TokLit (Int 1); TokBuiltInOp (Comp Eq); TokLit (Int 2)]
   Expect.equal (tokeniser "1+1==2") expected "Tokenise an invalid equation"

[<Tests>]
let Test5 =
 testCase "Equation test true with spaces" <| fun () ->
   let expected = [TokWhitespace Space; TokLit (Int 1); TokWhitespace Space; TokBuiltInOp (Arithm Add); TokWhitespace Space; TokLit (Int 1); TokWhitespace Space; TokSpecOp EQUALS; TokWhitespace Space; TokLit (Int 2); TokWhitespace Space]
   Expect.equal (tokeniser " 1 + 1 = 2 ") expected "Tokenise a valid equation with spaces between every character"

[<Tests>]
let Test6 =
 testCase "DashID: Subtract, Negative, NEGATE or ARROWFUNC 1" <| fun () ->
   let expected = [TokLit (Int 1); TokBuiltInOp (Arithm Subtract); TokLit (Int 1); TokSpecOp EQUALS; TokLit (Int 0)]
   Expect.equal (tokeniser "1-1=0") expected "Tokenise as subtract (no spaces)"

[<Tests>]
let Test7 =
 testCase "DashID: Subtract, Negative, NEGATE or ARROWFUNC 2" <| fun () ->
   let expected = [TokLit (Int 1); TokWhitespace Space; TokBuiltInOp (Arithm Subtract); TokWhitespace Space; TokLit (Int 1); TokWhitespace Space; TokSpecOp EQUALS; TokWhitespace Space; TokLit (Int 0)]
   Expect.equal (tokeniser "1 - 1 = 0") expected "Tokenise as subtract (spaces)"

[<Tests>]
let Test8 =
 testCase "DashID: Subtract, Negative, NEGATE or ARROWFUNC 3" <| fun () ->
   let expected = [TokLit (Int 1); TokWhitespace Space; TokLit (Int -1); TokWhitespace Space; TokSpecOp EQUALS; TokWhitespace Space; TokLit (Int 0)]
   Expect.equal (tokeniser "1 -1 = 0") expected "Tokenise as negative integer"
   
[<Tests>]
let Test9 =
 testCase "DashID: Subtract, Negative, NEGATE or ARROWFUNC 4" <| fun () ->
   let expected =  [TokLit (Int 1); TokBuiltInOp (Arithm Subtract); TokLit (Int -1); TokWhitespace Space; TokSpecOp EQUALS; TokWhitespace Space; TokLit (Int 0)]
   Expect.equal (tokeniser "1--1 = 0") expected "Tokenise as subtract followed by negative integer"

[<Tests>]
let Test10 =
 testCase "DashID: Subtract, Negative, NEGATE or ARROWFUNC 5" <| fun () ->
   let expected = [TokLit (Int 1); TokSpecOp ARROWFUNC; TokLit (Int 1); TokWhitespace Space; TokSpecOp EQUALS; TokWhitespace Space; TokLit (Int 0)]
   Expect.equal (tokeniser "1->1 = 0") expected "Tokenise as arrow"

[<Tests>]
let Test11 =
 testCase "DashID: Subtract, Negative, NEGATE or ARROWFUNC 6" <| fun () ->
   let expected = [TokLit (Int 1); TokWhitespace Space; TokSpecOp ARROWFUNC; TokBuiltInOp (Arithm Subtract); TokWhitespace Space; TokLit (Int 1); TokWhitespace Space; TokSpecOp EQUALS; TokWhitespace Space; TokLit (Int 0)]
   Expect.equal (tokeniser "1 ->- 1 = 0") expected "Tokenise as arrow followed by subtract"

[<Tests>]
let Test12 =
 testCase "DashID: Subtract, Negative, NEGATE or ARROWFUNC 7" <| fun () ->
   let expected = [TokLit (Int 1); TokWhitespace Space; TokSpecOp ARROWFUNC; TokLit (Int -1); TokWhitespace Space; TokSpecOp EQUALS; TokWhitespace Space; TokLit (Int 0)]
   Expect.equal (tokeniser "1 ->-1 = 0") expected "Tokenise as arrow followed by negative integer"

[<Tests>]   
let allTestsWithExpecto() =
   runTestsInAssembly defaultConfig [||]