<<<<<<< HEAD
﻿open System
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
=======
﻿open Testing
>>>>>>> 18430ecc3904b8a8bddd47917fc0902cd49d61d1

[<EntryPoint>]
let main(argv) =     

    // Run 9 unit test cases for parser
    parserTestsWithExpecto() |> ignore

    System.Console.ReadKey() |> ignore
<<<<<<< HEAD
>>>>>>> 5df67b029267b4fb1f2a63a81340acdc28700090
=======
>>>>>>> 18430ecc3904b8a8bddd47917fc0902cd49d61d1
    0 // return an integer exit code

