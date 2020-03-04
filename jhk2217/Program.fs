open System

open Lexer
open Patronum

[<EntryPoint>]
let main argv =
    printfn "%A" (tokeniser "[true,\"false\",1,2.3,\"4 5.6\",[7,8.9]] = 0")
    allTestsWithExpecto() |> ignore
    Console.ReadKey() |> ignore
    0 // return an integer exit code