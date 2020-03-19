module TestsLexer

open Expecto
open Helpers
open Lexer

[<Tests>]
let lexerTestListWithExpecto =
   testList "Lexer Tests with Expecto" [
      test "Lexer Test 1" {
         let expected = [TokLit (Int -1)]
         Expect.equal (tokeniser "-1") expected "Tokenise negative number"
      }
      
      test "Lexer Test 2" {
         let expected = [TokLit (Int -1)]
         Expect.equal (tokeniser "- 1") expected "Tokenise negative number - check ignoring spaces"
      }
        
        
      test "Lexer Test 3" {
         let expected = [TokUnaryOp NEGATE; TokIdentifier "x"]
         Expect.equal (tokeniser "-x") expected "Tokenise negated variable"
      }
      
      test "Lexer Test 4" {
         let expected = [TokLit (Int 1); TokBuiltInOp (Arithm Subtract); TokLit (Int 1)]
         Expect.equal (tokeniser "1 - 1") expected "Tokenise number minus number"
      }
        
      test "Lexer Test 5" {
         let expected = [TokLit (Int 1); TokBuiltInOp (Arithm Subtract); TokIdentifier "x"]
         Expect.equal (tokeniser "1 - x") expected "Tokenise number minus variable"
      }

      test "Lexer Test 6" {
         let expected = [TokIdentifier "x"; TokBuiltInOp (Arithm Subtract); TokLit (Int 1)]
         Expect.equal (tokeniser "x - 1") expected "Tokenise variable minus number"
      }

      test "Lexer Test 7" {
         let expected = [TokIdentifier "x"; TokBuiltInOp (Arithm Subtract); TokIdentifier "x"]
         Expect.equal (tokeniser "x - x") expected "Tokenise variable minus variable"
      }
        
      test "Lexer Test 8" {
         let expected = [TokLit (Int 1); TokBuiltInOp (Arithm Subtract); TokLit (Int -1)]
         Expect.equal (tokeniser "1 - - 1") expected "Tokenise number minus negative number"
      }

      test "Lexer Test 9" {
         let expected = [TokLit (Int 1); TokBuiltInOp (Arithm Subtract); TokUnaryOp NEGATE; TokIdentifier "x"]
         Expect.equal (tokeniser "1 - - x") expected "Tokenise number minus negated variable"
      }
        
      test "Lexer Test 10" {
         let expected = [TokIdentifier "x"; TokBuiltInOp (Arithm Subtract); TokLit (Int -1)]
         Expect.equal (tokeniser "x - - 1") expected "Tokenise variable minus negative number"
      }
          
      test "Lexer Test 11" {
         let expected = [TokIdentifier "x"; TokBuiltInOp (Arithm Subtract); TokUnaryOp NEGATE; TokIdentifier "x"]
         Expect.equal (tokeniser "x - - x") expected "Tokenise variable minus negated variable"
      }
        
      test "Lexer Test 12" {
         let expected = [TokLit (Int -1); TokBuiltInOp (Arithm Subtract); TokLit (Int -1)]
         Expect.equal (tokeniser "- 1 - - 1") expected "Tokenise negative number minus negative number"
      }

      test "Lexer Test 13" {
         let expected = [TokLit (Int -1); TokBuiltInOp (Arithm Subtract); TokUnaryOp NEGATE; TokIdentifier "x"]
         Expect.equal (tokeniser "- 1 - - x") expected "Tokenise negative number minus negated variable"
      }

      test "Lexer Test 14" {
         let expected = [TokUnaryOp NEGATE; TokIdentifier "x"; TokBuiltInOp (Arithm Subtract); TokLit (Int -1)]
         Expect.equal (tokeniser "- x - - 1") expected "Tokenise negated variable minus negative number"
      }
        
      test "Lexer Test 15" {
         let expected = [TokUnaryOp NEGATE; TokIdentifier "x"; TokBuiltInOp (Arithm Subtract); TokUnaryOp NEGATE; TokIdentifier "x"]
         Expect.equal (tokeniser "- x - - x") expected "Tokenise negated variable minus negated variable"
      }
   ]

let lexerTestsWithExpecto() =
    runTests defaultConfig lexerTestListWithExpecto |> ignore