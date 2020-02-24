open System
<<<<<<< HEAD
open Parser

let print x = printfn "%A" x

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    print <| deleteThisSample()

    Console.ReadKey() |> ignore // added to prevent program for just ending
=======
// open Tokeniser
open Common
open Parser

// mock parser example from Don Syme
let src1 = "2x^2+3x+5"
let src2 = "2+5"
let print x = printfn "%A" x

[<EntryPoint>]
let main(argv) =
    // builtinPlus a b -> FuncApp(FuncApp(builtinPlus, a), b)
    let inp1 = [TokLit (Int 5); TokArithmeticOp ADD; TokLit (Int 10)]
    let inp2 = [TokLit (Int 5)]
    let inp3 = [TokArithmeticOp ADD]
    let inp4 = []    
    let inp5 = [TokLit (Double 5.5); TokLit (String "hi"); TokLit (Int 10)]    
    // This is what simple function application should look like for two arguments

    let p = 
        parser {
            let! first = pToken
            let! second = pToken
            let! third = pToken
            return first, second, third
        }

    pRun pLiteral inp5 |> printfn "%A"

    pRun (pMany pLiteral) inp5 |> printfn "%A"

    pRun (pMany pAST) inp1 |> printfn "%A"

    System.Console.ReadKey() |> ignore
>>>>>>> 5df67b029267b4fb1f2a63a81340acdc28700090
    0 // return an integer exit code

