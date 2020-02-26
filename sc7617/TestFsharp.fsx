let print x = printf "%A \n" x 



let map = Map.empty

let mapAdd1 = map.Add("1", "one")
let mapAdd2 = mapAdd1.Add("2", "two")


let str1 = "Hello"
let str2 = "World"
let int1 = 1
let int2 = 2

print <| not (str1 = str1)
print <| mapAdd2.["1"]
print <| mapAdd2.["2"]
print <| Map.tryFind ("3") mapAdd2
