open Expecto
open Lexer
open Parser
open Combinator_runtime
open Helpers

let expectoConfig = { defaultConfig with verbosity = Logging.LogLevel.Debug }

//let Interpreter std_in =
//    std_in
//    |> tokeniser
//    |> (pRun pExpr)
//    |> Interpret

let x =
       FuncDefExp("f",
              Lambda("y",
                     FuncDefExp("x", Literal (Int 2), DCall(DCall(BuiltInFunc (Arithm Multiply), Variable "x"), Variable "y")))
              , DCall(Variable "f", Literal (Int 3)))


[<EntryPoint>]
let main argv =
       let testString1 = "2+2"
       let testString2 = "def x = x + 1" // deftok xtok equalstok xtok plustok 1tok
       let testString3 = "def x a b = (a,b) \n x 1 2" // PROBLEM HERE
       let testString4 = "[a,b]"
       let testString5 = "def x f = f + 1 \n \t x 1" // deftok xtok ftok equalstok ftok plustok 1tok \ntok xtok
       let testString6 = "def f x y = 2 * (x + y) \n f 2 3" /// PROBLEM HERE

       // let tokenTest1 = [TokSpecOp DEF ; TokIdentifier ("f") ; TokIdentifier ("y") ; TokSpecOp EQUALS ; TokWhitespace LineFeed ; TokSpecOp DEF ; TokIdentifier ("x") ; TokSpecOp EQUALS ; TokLit (Int 3) ; TokWhitespace LineFeed ; TokIdentifier ("x") ; TokBuiltInOp (Arithm Multiply) ; TokIdentifier ("y") ; TokWhitespace LineFeed ; TokIdentifier ("f") ; TokLit (Int 2)]
       printf "\ndef f x y = 2 * x + y \n f 2 3\n %A\n" <| ("def f x y = 2 * x + y \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret)
       printf "-----------------------------------"
       printf "\ndef f x y = (2 * x + y) \n f 2 3\n %A\n" <| ("def f x y = (2 * x + y) \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret)
       printf "-----------------------------------"
       printf "start\n"
       printf "\ndef f x y = 2 * (x + y) \n f 2 3\n %A\n" <| ("def f x y = 2 * (x + y) \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret)
       printf "end\n"
       printf "-----------------------------------"
       printf "\ndef f x y = (2 * x) + y \n f 2 3\n %A\n" <| ("def f x y = (2 * x) + y \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret)
       printf "-----------------------------------"
       printf "\ndef f x y = ((2 * x) + y) \n f 2 3\n %A\n" <| ("def f x y = ((2 * x) + y) \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret)
       printf "-----------------------------------"
       printf "\ndef f x y = (2 * (x + y)) \n f 2 3\n %A\n" <| ("def f x y = (2 * (x + y)) \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret)
       printf "-----------------------------------"
       printf "\ndef f x y = (2 * ((x) + (y))) \n f 2 3\n %A\n" <| ("def f x y = (2 * ((x) + (y))) \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret)

       let emptyPair = [TokSpecOp LSB ; TokSpecOp RSB]
       let singlePair = [TokSpecOp LSB ; TokLit (Int 1) ; TokSpecOp RSB]
       let twoPair = [TokSpecOp LSB ; TokLit (Int 5) ; TokSpecOp COMMA ; TokLit (Int 6) ; TokSpecOp RSB]
       let threePair = [TokSpecOp LSB ; TokLit (Int 2) ; TokSpecOp COMMA ; TokLit (Int 3) ; TokSpecOp COMMA ; TokLit (Int 4) ; TokSpecOp RSB]
       
       printf "\n%A" <| (pRun pExpr emptyPair)
       printf "\n%A" <| (pRun pExpr singlePair)
       printf "\n%A" <| (pRun pExpr twoPair)
       printf "\n%A" <| (pRun pExpr threePair)
       // this shoulf fail -> if the index is not equal to the number of tokens then it should fail
//       printf "\n%A" <| ("((5)))" |> tokeniser |> pRun pExpr)
//       printf "\n%A" <| ("(((5)))" |> tokeniser |> pRun pExpr)

       // printf "%A\n" <| (testString2 |> tokeniser)
       // printf "%A\n" <| (testString3 |> tokeniser)
       // printf "%A\n" <| (testString4 |> tokeniser)
       // printf "%A\n" <| (testString5 |> tokeniser)
       // function application of lambdas fails (lambda x -> x + 1) 2
       // let tokenTest2 = [TokSpecOp LAMBDA ; TokIdentifier "x" ; TokSpecOp ARROWFUNC ; TokIdentifier "x" ; TokBuiltInOp (Arithm Add) ; TokLit (Int 1) ; TokLit (Int 2)]
       // printf "%A" <| (pRun pExpr tokenTest2 |> Interpret)

       // let tokenTest3 = [TokSpecOp LRB ; TokSpecOp LAMBDA ; TokIdentifier "x" ; TokSpecOp ARROWFUNC ; TokIdentifier "x" ; TokBuiltInOp (Arithm Add) ; TokLit (Int 1) ; TokSpecOp RRB ; TokLit (Int 2)]
       // printf "\n %A" <| (pRun pExpr tokenTest3  |> Interpret)
       // printf "\n \n \n \n"
       
       // // FuncDefExp('f', Lambda('x', Lambda('y', AST=== 2*x)), Call(Call('f','2'), '3')))
       // let example_5 = """
       // def f x y = 2*x+y
       // f 2 3
       // """

       // let tokenTest4 = [TokSpecOp DEF ; TokIdentifier "f" ; TokIdentifier "x" ; TokIdentifier "y" ; TokSpecOp EQUALS ; TokLit (Int 2) ; TokBuiltInOp (Arithm Multiply) ; TokIdentifier "x" ;  TokBuiltInOp (Arithm Add); TokIdentifier "y" ; TokWhitespace LineFeed ; TokIdentifier "f" ; TokLit (Int 2) ; TokLit (Int 3)]
       // printf "\n %A" <| (pRun pExpr tokenTest4 |> Interpret)

//     let tokenTest5 = [TokSpecOp LRB ; TokSpecOp LAMBDA ; TokIdentifier "x" ; TokSpecOp ARROWFUNC ; TokIdentifier "x" ; TokBuiltInOp (Arithm Add) ; TokLit (Int 1) ; TokSpecOp RRB ; TokLit (Int 2)]
//     printf "%A" <| (pRun pExpr tokenTest5)

//     let tokenTest6 = [TokSpecOp LRB ; TokSpecOp LAMBDA ; TokIdentifier "x" ; TokSpecOp ARROWFUNC ; TokIdentifier "x" ; TokBuiltInOp (Arithm Add) ; TokLit (Int 1) ; TokSpecOp RRB ; TokLit (Int 2)]
//     printf "%A" <| (pRun pExpr tokenTest6)

//     let tokenTest7 = [TokSpecOp LRB ; TokSpecOp LAMBDA ; TokIdentifier "x" ; TokSpecOp ARROWFUNC ; TokIdentifier "x" ; TokBuiltInOp (Arithm Add) ; TokLit (Int 1) ; TokSpecOp RRB ; TokLit (Int 2)]
//     printf "%A" <| (pRun pExpr tokenTest7)

//     let tokenTest8 = [TokSpecOp LRB ; TokSpecOp LAMBDA ; TokIdentifier "x" ; TokSpecOp ARROWFUNC ; TokIdentifier "x" ; TokBuiltInOp (Arithm Add) ; TokLit (Int 1) ; TokSpecOp RRB ; TokLit (Int 2)]
//     printf "%A" <| (pRun pExpr tokenTest8)
    // runTestsInAssemblyWithCLIArgs [] [||] |> ignore
       System.Console.ReadKey() |> ignore
       0