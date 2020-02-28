

open System
//open ParseHelpers

open Expecto
open FsCheck


// helpful functions


let print x = printf "%A \n" x
let printPipe x = printf "%A \n" x ; x





let tokenOpArray = [|"/";"*";"+";"-";"/";"(";")";";";",";"{";"}"|]
let tokenEndStrings = [|";"|]
// custom random generators using FsCheck.Gen combinators
// See https://github.com/haf/expecto/blob/master/README.md#property-based-tests
// See https://fscheck.github.io/FsCheck/TestData.html for Gen.* info
let alphaGen = Gen.elements ([ 'a'..'z'] @ ['A'..'Z']) // choose random element of list
let digitGen = Gen.elements(['0'..'9'])
let alphaNumGen = Gen.oneof [alphaGen ; digitGen] // uniform randomly select a generator from list
let regNameGen = Gen.map (sprintf "R%d") (Gen.oneof [Gen.choose(0,15)])
let manyAlphaNumGen = 
    // NB the simpler a |> Seq.ofArray |> string here does not work. Bug?
    let arrToStr a = a |> Array.map Char.ToString |> String.concat "" // convert array of Char to string
    Gen.arrayOf alphaNumGen // randomly sized array of alphaNumGen (length scales with 'size' parameter)
    |> Gen.map arrToStr // map result of generator to make new generator
let idGen = Gen.map2 (fun (a:char) b -> 
    a.ToString() + b) alphaGen manyAlphaNumGen // combine initial letter and variable number of alphanum
let opGen = Gen.elements tokenOpArray // random choice from list
let numGen = Gen.map (sprintf "%d") (Gen.choose(0,9999999)) // random choice from integer range
let tokListGen = 
    Gen.oneof [idGen ; idGen ; numGen; numGen; opGen] // randomly choose token type
    |> Gen.listOf // generate random length lists of tokens (similarly to arrayOf)
let optSpaceGen = Gen.elements ["";" "]



[<EntryPoint>]
let main argv =

    print <| alphaGen

    printfn "press any key to terminate"
    Console.ReadKey() |> ignore
    0 