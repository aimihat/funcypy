open System
// open Tokeniser
open Parser

// mock parser example from Don Syme
let src1 = "2x^2+3x+5"
let src2 = "2+5"
let print x = printfn "%A" x

[<EntryPoint>]
let main(argv) =
    // let tokenResult = tokenise src1    
    // print <| tokenResult

    let parseResult0 = parse src1
    print <| parseResult0

    let parseResult1 = parse src2
    print <| parseResult1

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code

