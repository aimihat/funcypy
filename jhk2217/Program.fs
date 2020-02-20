open System

type Token = Chr of char | Other of string

let tokens = ['.';'(';')';'[';']']

let rec charstring (clst: char list) =
    match clst with
    | hd::tl -> (string hd)+(charstring tl)
    | _ -> ""

let parseChar lst =
    match lst with
    | hd::rest when (List.exists (fun el -> el = hd) tokens) -> Some (Chr hd), Ok rest
    | _ -> None, Error <| sprintf "%A is not one of the tokens" 

let rec tokeniseT3 (str: string) =
    let rec buildstring (acc,lst) =
        match lst with
        | hd::tl when not (List.exists (fun el -> el = hd) tokens) -> buildstring (List.append acc [hd],tl)
        | _ -> (acc,lst)
    let rec tokeniser lst =
        match lst with
        | hd::tl when (List.exists (fun el -> el = hd) tokens) -> List.append [Chr hd] (tokeniser tl)
        | hd::tl when not (List.exists (fun el -> el = hd) tokens) -> 
            match buildstring ([],lst) with
            | (matched,left) -> List.append [Other (charstring matched)] (tokeniser left)
        | _ -> []
    tokeniser (Seq.toList str)
