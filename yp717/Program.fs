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
open Tokeniser

// mock parser example from Don Syme
let src1 = "2x^2+3x+5"
let src2 = "2+5"
let print x = printfn "%A" x

[<EntryPoint>]
let main(argv) =
    // let tokenResult = tokenise src1    
    // print <| tokenResult

    let tokenResult = tokenise "([.])"
    print <| "Result is: "
    print <| tokenResult

    // let parseResult0 = parse src1
    // print <| parseResult0

    // let parseResult1 = parse src2
    // print <| parseResult1

    System.Console.ReadKey() |> ignore
>>>>>>> 5df67b029267b4fb1f2a63a81340acdc28700090
    0 // return an integer exit code

