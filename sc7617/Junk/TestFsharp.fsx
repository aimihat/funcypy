let print x = printf "%A \n" x 



let map = Map.empty

let mapAdd1 = map.Add("1", "one")
let mapAdd2 = mapAdd1.Add("2", "two")
let mapAdd3 = mapAdd2.Add("1", "ONE")


let str1 = "Hello"
let str2 = "World"
let int1 = 1
let int2 = 2

// print <| not (str1 = str1)
// print <| mapAdd2.["1"]
// print <| mapAdd2.["2"]
// print <| mapAdd3.["1"]
// print <| Map.tryFind ("3") mapAdd2



let matchStr (inp: string) = 
    let strLst = (inp.Split ':') |> Array.toList
    let expected = "No valid expression supplied for lambda calculation. Evaluated pure lambda is"
    match strLst with
    | h :: rest when h = expected -> "Yes"
    | _ -> "No"

print <| matchStr "No valid expression supplied for lambda calculation. Evaluated pure lambda"