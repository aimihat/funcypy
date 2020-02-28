open System

open Lexer

[<EntryPoint>]
let main argv =
    "[[true,false,[1,2],[3,4,5,6,7,[8,9,8,7],6,[5,4,[3,2]]]],1,-2,-3,-4.5,6]"
    |> tokeniser
    |> printfn "%A"
    0
