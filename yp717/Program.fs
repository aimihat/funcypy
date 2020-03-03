open Testing

[<EntryPoint>]
let main(argv) =     

    // Run 9 unit test cases for parser
    parserTestsWithExpecto() |> ignore

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code

