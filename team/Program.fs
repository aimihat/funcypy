open EndToEndTests
open ParserTests
open Lexer
open Parser
open Combinator_runtime
 
[<EntryPoint>]
let main(argv) =     
    endToEndTestsWithExpecto() |> ignore
    parserTestsWithExpecto() |> ignore
    // printf "\ndef f x y = 2 * x + y \n f 2 3\n %A\n" <| ("def f x y = 2 * x + y \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret)
    
    // printf "\ndef f x y = 2 * x + y \n f 2 3\n %A\n" <| ("def f x y = 2 * x + y \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret)
    // printf "-----------------------------------"
    // printf "\ndef f x y = (2 * x + y) \n f 2 3\n %A\n" <| ("def f x y = (2 * x + y) \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret)
    // printf "-----------------------------------"
    // printf "\ndef f x y = 2 * (x + y) \n f 2 3\n %A\n" <| ("def f x y = 2 * (x + y) \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret)
    // printf "-----------------------------------"
    printf "\ndef f x y = (2 * x) + y \n f 2 3\n %A\n" <| ("def f x y = (2 * x) + y \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret)
    // printf "-----------------------------------"
    // printf "\ndef f x y = ((2 * x) + y) \n f 2 3\n %A\n" <| ("def f x y = ((2 * x) + y) \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret)
    // printf "-----------------------------------"
    // printf "\ndef f x y = (2 * (x + y)) \n f 2 3\n %A\n" <| ("def f x y = (2 * (x + y)) \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret)
    // printf "-----------------------------------"
    // printf "\ndef f x y = (2 * ((x) + (y))) \n f 2 3\n %A\n" <| ("def f x y = (2 * ((x) + (y))) \n f 2 3" |> tokeniser |> pRun pExpr |> Interpret)

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code