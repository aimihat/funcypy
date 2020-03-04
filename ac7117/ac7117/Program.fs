open Expecto

let expectoConfig = { defaultConfig with verbosity = Logging.LogLevel.Debug }

[<EntryPoint>]
let main argv =
    runTestsInAssemblyWithCLIArgs [] [||] |> ignore
    0