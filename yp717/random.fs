// Random Built in Functions that may be cool to integrate with the rest of the project

// Complete Active Pattern to check if number is odd or even
let (|ODD|EVEN|) number = 
    if number % 2 = 0
    then EVEN
    else ODD

// This checks if a string contains an int
let (|Integer|_|) str =
  match Int32.TryParse(str) with
  | (true, i) -> Some i
  | _ -> None

// Checks if a number is positive negative or zero
let (|Positive|Negative|Zero|) num = 
    if num > 0 then Positive 
    elif num < 0 then Negative
    else Zero

let Sign value = 
    match value with
    | Positive -> printf "%d is positive" value
    | Negative -> printf "%d is negative" value
    | Zero -> printf "The value is zero"