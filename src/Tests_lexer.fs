module TestsLexer

open Expecto
open Helpers
open Lexer

[<Tests>]
let lexerTestListWithExpecto =
    testList "Lexer Tests with Expecto" [
      test "Lexer Test 0.1" {
         let expected = [TokLit (Int 12345)]
         Expect.equal (tokeniser "12345") expected "basic test: tokenise an integer"
      }

      test "Lexer Test 0.2" {
         let expected = [TokLit (Double 123.45)]
         Expect.equal (tokeniser "123.45") expected "basic test: tokenise a double"
      }

      test "Lexer Test 0.3" {
         let expected = [TokLit (Bool true)]
         Expect.equal (tokeniser "true") expected "basic test: tokenise a boolean"
      }

      test "Lexer Test 0.4" {
         let expected = [TokLit (String "this is a string")]
         Expect.equal (tokeniser "\"this is a string\"") expected "basic test: tokenise a string"
      }

      test "Lexer Test 0.5" {
         let expected = [TokBuiltInOp (Arithm Add)]
         Expect.equal (tokeniser "+") expected "basic test: tokenise an arithmetic operator"
      }

      test "Lexer Test 0.6" {
         let expected = [TokBuiltInOp (Comp Eq)]
         Expect.equal (tokeniser "==") expected "basic test: tokenise a comparison operator"
      }

      test "Lexer Test 0.7" {
         let expected = [TokWhitespace LineFeed]
         Expect.equal (tokeniser "\n") expected "basic test: tokenise LineFeed"
      }

      test "Lexer Test 0.8" {
         let expected = [TokSpecOp LAMBDA]
         Expect.equal (tokeniser "lambda") expected "basic test: tokenise an operator"
      }

      test "Lexer Test 0.9" {
         let expected = [TokUnaryOp NOT]
         Expect.equal (tokeniser "not") expected "basic test: tokenise a unary operator"
      }

      test "Lexer Test 1.1" {
         let expected = [TokLit (Double 12345.0)]
         Expect.equal (tokeniser "12345.") expected "buildNum: build double (no value after decimal)"
      }

      test "Lexer Test 1.2" {
         let expected = [TokLit (Double 0.12345)]
         Expect.equal (tokeniser ".12345") expected "buildNum: build double (no value before decimal)"
      }

      test "Lexer Test 1.3" {
         let expected = [TokLit (Int 12345)]
         Expect.equal (tokeniser "12345") expected "buildNum: build integer"
      }
      
      test "Lexer Test 1.4" {
         let expected = [TokLit (Double 12345.0)]
         Expect.equal (tokeniser "12345.") expected "buildNum: build double (no value after decimal)"
      }
      
      test "Lexer Test 1.5" {
         let expected = [TokLit (Double 123.45)]
         Expect.equal (tokeniser "123.45") expected "buildNum: build double (normal decimal)"
      }

      test "Lexer Test 1.6" {
         let expected = [TokLit (Double 0.12345)]
         Expect.equal (tokeniser ".12345") expected "buildNum: build double (no value before decimal)"
      }
      
      test "Lexer Test 2.1" {
         let expected = [TokLit (String "1 2.3 true + == lambda")]
         Expect.equal (tokeniser "\"1 2.3 true + == lambda\"") expected "buildString: build string instead of tokens"
      }
      
      test "Lexer Test 2.2" {
         let expected = [TokLit (String "this"); TokBuiltInOp (Arithm Add); TokLit (String "is"); TokLit (String "a"); TokSpecOp LSB; TokLit (String "string"); TokSpecOp RSB]
         Expect.equal (tokeniser "\"this\"+\"is\"\"a\"[\"string\"]") expected "buildString: build multiple strings with interruptions"
      }
      
      test "Lexer Test 3.1" {
         let expected = [TokLit (Int -1)]
         Expect.equal (tokeniser "-1") expected "dashID: tokenise negative number"
      }
      
      test "Lexer Test 3.2" {
         let expected = [TokLit (Int -1)]
         Expect.equal (tokeniser "- 1") expected "dashID: tokenise negative number - check ignoring spaces"
      }
      
      test "Lexer Test 3.3" {
         let expected = [TokUnaryOp NEGATE; TokIdentifier "x"]
         Expect.equal (tokeniser "-x") expected "dashID: tokenise negated variable"
      }
      
      test "Lexer Test 3.4" {
         let expected = [TokLit (Int 1); TokBuiltInOp (Arithm Subtract); TokLit (Int 1)]
         Expect.equal (tokeniser "1 - 1") expected "dashID: tokenise number minus number"
      }
      
      test "Lexer Test 3.5" {
         let expected = [TokLit (Int 1); TokBuiltInOp (Arithm Subtract); TokIdentifier "x"]
         Expect.equal (tokeniser "1 - x") expected "dashID: tokenise number minus variable"
      }
      
      test "Lexer Test 3.6" {
         let expected = [TokIdentifier "x"; TokBuiltInOp (Arithm Subtract); TokLit (Int 1)]
         Expect.equal (tokeniser "x - 1") expected "dashID: tokenise variable minus number"
      }
      
      test "Lexer Test 3.7" {
         let expected = [TokIdentifier "x"; TokBuiltInOp (Arithm Subtract); TokIdentifier "x"]
         Expect.equal (tokeniser "x - x") expected "dashID: tokenise variable minus variable"
      }
      
      test "Lexer Test 3.8" {
         let expected = [TokLit (Int 1); TokBuiltInOp (Arithm Subtract); TokLit (Int -1)]
         Expect.equal (tokeniser "1 - - 1") expected "dashID: tokenise number minus negative number"
      }
      
      test "Lexer Test 3.9" {
         let expected = [TokLit (Int 1); TokBuiltInOp (Arithm Subtract); TokUnaryOp NEGATE; TokIdentifier "x"]
         Expect.equal (tokeniser "1 - - x") expected "dashID: tokenise number minus negated variable"
      }
      
      test "Lexer Test 3.10" {
         let expected = [TokIdentifier "x"; TokBuiltInOp (Arithm Subtract); TokLit (Int -1)]
         Expect.equal (tokeniser "x - - 1") expected "dashID: tokenise variable minus negative number"
      }
      
      test "Lexer Test 3.11" {
         let expected = [TokIdentifier "x"; TokBuiltInOp (Arithm Subtract); TokUnaryOp NEGATE; TokIdentifier "x"]
         Expect.equal (tokeniser "x - - x") expected "dashID: tokenise variable minus negated variable"
      }
      
      test "Lexer Test 3.12" {
         let expected = [TokLit (Int -1); TokBuiltInOp (Arithm Subtract); TokLit (Int -1)]
         Expect.equal (tokeniser "- 1 - - 1") expected "dashID: tokenise negative number minus negative number"
      }

      test "Lexer Test 3.13" {
         let expected = [TokLit (Int -1); TokBuiltInOp (Arithm Subtract); TokUnaryOp NEGATE; TokIdentifier "x"]
         Expect.equal (tokeniser "- 1 - - x") expected "dashID: tokenise negative number minus negated variable"
      }

      test "Lexer Test 3.14" {
         let expected = [TokUnaryOp NEGATE; TokIdentifier "x"; TokBuiltInOp (Arithm Subtract); TokLit (Int -1)]
         Expect.equal (tokeniser "- x - - 1") expected "dashID: tokenise negated variable minus negative number"
      }

      test "Lexer Test 3.15" {
         let expected = [TokUnaryOp NEGATE; TokIdentifier "x"; TokBuiltInOp (Arithm Subtract); TokUnaryOp NEGATE; TokIdentifier "x"]
         Expect.equal (tokeniser "- x - - x") expected "dashID: tokenise negated variable minus negated variable"
      }

      test "Lexer Test 3.16" {
         let expected = [TokSpecOp IF; TokSpecOp LRB ; TokIdentifier "x"; TokBuiltInOp (Comp Lt) ; TokLit (Int 1) ; TokSpecOp RRB ; TokSpecOp COLON ; TokWhitespace LineFeed ; TokLit (Bool true) ; TokWhitespace LineFeed ; TokSpecOp ELSE ; TokSpecOp COLON ; TokWhitespace LineFeed ; TokLit (Bool false)]
         Expect.equal (tokeniser "if (x<1): \n true \n else: \n false") expected "if statement with all new lines"
      }

      test "Lexer Test 3.17" {
         let expected = [TokSpecOp IF; TokSpecOp LRB ; TokIdentifier "x"; TokBuiltInOp (Comp Lt) ; TokLit (Int 1) ; TokSpecOp RRB ; TokSpecOp COLON ; TokLit (Bool true) ; TokWhitespace LineFeed ; TokSpecOp ELSE ; TokSpecOp COLON ; TokLit (Bool false)]
         Expect.equal (tokeniser "if (x<1): true \n else: false") expected "if statement with some new lines"
      }

      test "Lexer Test with comments" {
         let expected = [TokSpecOp IF; TokSpecOp LRB ; TokIdentifier "x"; TokBuiltInOp (Comp Lt) ; TokLit (Int 1) ; TokSpecOp RRB ; TokSpecOp COLON ; TokLit (Bool true) ; TokWhitespace LineFeed ; TokSpecOp ELSE ; TokSpecOp COLON ; TokLit (Bool false)]
         Expect.equal (tokeniser "// comments are awesome \n if (x<1): true \n else: false") expected "if statement with some new lines"
      }

      test "Lexer Test with comments 2" {
         let expected = [TokSpecOp IF; TokSpecOp LRB ; TokIdentifier "x"; TokBuiltInOp (Comp Lt) ; TokLit (Int 1) ; TokSpecOp RRB ; TokSpecOp COLON ; TokLit (Bool true) ; TokWhitespace LineFeed ; TokSpecOp ELSE ; TokSpecOp COLON ; TokLit (Bool false)]
         Expect.equal (tokeniser "// comments are awesome \n if (x<1): true \n else: false // comments are still awesome \n") expected "if statement with some new lines"
      }
   ]

let lexerTestsWithExpecto() =
    runTests defaultConfig lexerTestListWithExpecto |> ignore