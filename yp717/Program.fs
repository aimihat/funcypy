open System
open Parser

let print x = printfn "%A" x

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    print <| deleteThisSample()

    Console.ReadKey() |> ignore // added to prevent program for just ending
    0 // return an integer exit code
