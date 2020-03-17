module Lexer
open Helpers

let intChars = ['0' .. '9']
let alphaNumeric = ['a' .. 'z'] @ ['A' .. 'Z'] @ intChars
let boolMap = Map ["true",true;"false",false]
let mathMap = Map ["+", Arithm Add; "-", Arithm Subtract;"*", Arithm Multiply;"/", Arithm Divide;"==",Comp Eq;"!=",Comp Ne;"<",Comp Lt;">",Comp Gt;"<=",Comp Le;">=",Comp Ge]
let spaceMap = Map ["\n",LineFeed]
let opMap = Map ["[",LSB;"]",RSB;"(",LRB;",",COMMA;"if",IF;"=",EQUALS;"def",DEF;"then",THEN;"else",ELSE;")",RRB;"lambda",LAMBDA;"->",ARROWFUNC]
let unaryMap = Map ["not",NOT;"-",NEGATE]

let keys map =
    map
    |> Map.toList
    |> List.map fst
    
let rec charstring (clst: char list) =
    match clst with
    | hd::tl -> (string hd)+(charstring tl)
    | _ -> ""

let inMap map el =
    keys map |> List.exists (fun x -> x = el)

let rec skipSpaces lst =
    match lst with
    | hd::tl when hd = ' ' -> skipSpaces tl
    | newlst -> newlst

let rec skipSpaceToks lst =
    match lst with
    | hd::tl when hd = TokWhitespace Space -> skipSpaceToks tl
    | newlst -> newlst

let printPipe x = printfn "%A" x; x

let tokeniser (str: string) =
    //buildNum: given that the first char is a number, build either int or float
    let buildNum (tokLst,otherLst,positive) =
        let rec buildInts (acc,lst) =
            match lst with
            | hd::tl when List.exists (fun el -> el = hd) intChars -> buildInts (acc @ [hd],tl)
            | _ -> (acc,lst)
        match buildInts ([],otherLst) with
        | (intTok,hd::tl) when hd = '.' && positive -> 
            match buildInts ([],tl) with
            | ([],tl) -> (tokLst @ [intTok |> charstring |> float |> Double |> TokLit],tl)
            | (decLst,newOtherLst) -> (tokLst @ [intTok @ ['.'] @ decLst |> charstring |> float |> Double |> TokLit],newOtherLst)
        | (intTok,newOtherLst) when positive -> (tokLst @ [intTok |> charstring |> int |> Int |> TokLit],newOtherLst)
        | (intTok,hd::tl) when hd = '.' -> 
            match buildInts ([],tl) with
            | ([],tl) -> (tokLst @ [['-'] @ intTok |> charstring |> float |> Double |> TokLit],tl)
            | (decLst,newOtherLst) -> (tokLst @ [['-'] @ intTok @ ['.'] @ decLst |> charstring |> float |> Double |> TokLit],newOtherLst)
        | (intTok,newOtherLst) -> (tokLst @ [['-'] @ intTok |> charstring |> int |> Int |> TokLit],newOtherLst)
    //extractWord: get the next "word", defined by all the characters between two spaces/current space until the end of string
    let rec extractWord (acc,lst) =
        match lst with
        | hd::tl when List.exists (fun el -> el = hd) (alphaNumeric) -> extractWord (acc @ [hd],tl)
        | hd::tl -> (charstring acc,hd::tl)
        | [] -> (charstring acc,[])
    //buildString: a " has been found, build string until another " is found
    let buildString (tokLst,otherLst) =
        let rec stringChr (acc,lst) =
            match lst with
            | hd::tl when not (hd = '\"') -> stringChr (acc @ [hd],tl)
            | hd::tl -> (charstring acc |> String |> TokLit,tl)
            | [] -> (charstring acc |> String |> TokLit,lst) //This should never occur since there is guaranteed to be a quotation mark in lst due to the if-then-else statement
        let stringOpt (toks,others) =
            if (List.exists(fun el -> el = '\"') others)
            then match stringChr ([],others) with (strTok,newOthers) -> Some (toks @ [strTok]), Ok newOthers
            else None, Error <| sprintf "There is a string that has no ending quotation mark!"
        match stringOpt (tokLst,otherLst) with
        | Some newTokLst, Ok newOtherLst -> newTokLst,newOtherLst
        | None, Ok newOtherLst -> (tokLst @ [TokLit (String "")],newOtherLst) //This should never occur since newTokLst is at the very least the existing list tokLst
        | _, Error msg -> failwithf "%A" msg
    let rec tokenise (tokLst,otherLst) =
        let rec dashID (toks,others) = //must figure out if '-' is negative sign, NEGATE or SUBTRACT
            match others with
            | hd::tl when hd = '>' -> (toks @ [TokSpecOp (ARROWFUNC)],tl) // ->
            | hd::tl when List.exists (fun el -> el = hd) intChars ->
                match tokLst |> List.rev |> List.head with // #-#
                | TokLit (Int _) -> (toks @ [TokBuiltInOp (Arithm Subtract)],others)
                | TokLit (Double _) -> (toks @ [TokBuiltInOp (Arithm Subtract)],others)
                | _ -> buildNum (toks,others,false)
            | hd::tl when hd = ' ' -> dashID (toks,tl)
            | hd::tl when inMap mathMap (string hd) -> // - +
                match tokLst |> List.rev |> List.head with // #-#
                | TokLit (Int _) -> (toks @ [TokBuiltInOp (Arithm Subtract)],others)
                | TokLit (Double _) -> (toks @ [TokBuiltInOp (Arithm Subtract)],others)
                | _ -> buildNum (toks,others,false)
            | hd1::hd2::tl when List.exists (fun el -> el = charstring [hd1;hd2]) (keys mathMap) -> // - ==
                match tokLst |> List.rev |> List.head with // #-#
                | TokLit (Int _) -> (toks @ [TokBuiltInOp (Arithm Subtract)],others)
                | TokLit (Double _) -> (toks @ [TokBuiltInOp (Arithm Subtract)],others)
                | _ -> buildNum (toks,others,false)
            | [] -> //reached end of string
                match tokLst |> List.rev |> skipSpaceToks |> List.head with // # -
                | TokLit (Int _) -> (toks @ [TokBuiltInOp (Arithm Subtract)],others)
                | TokLit (Double _) -> (toks @ [TokBuiltInOp (Arithm Subtract)],others)
                | TokBuiltInOp _ -> (toks @ [TokBuiltInOp (Arithm Subtract)],others)
                | _ -> (toks @ [TokUnaryOp (NEGATE)],others)
            | _ -> (toks @ [TokUnaryOp (NEGATE)],others)
        match otherLst with
        | hd::tl when hd = ' ' -> tokenise (tokLst,tl)
        | hd::tl when hd = '\t' -> tokenise (tokLst,tl)
        | hd::tl when hd = '\v' -> tokenise (tokLst,tl)
        | hd::tl when hd = '\r' -> tokenise (tokLst,tl)
        | hd::tl when hd = '\f' -> tokenise (tokLst,tl)
        | hd::tl when hd = '-' -> tokenise (dashID (tokLst,tl))
        | hd::tl when hd = '\"' -> tokenise (buildString (tokLst,tl))
        | hd::tl when List.exists (fun el -> el = hd) intChars -> tokenise (buildNum (tokLst,otherLst,true))
        | hd::tl when List.exists (fun el -> el = string hd) (keys mathMap) -> tokenise (tokLst @ [TokBuiltInOp mathMap.[string hd]],tl)
        | hd1::hd2::tl when List.exists (fun el -> el = charstring [hd1;hd2]) (keys mathMap) -> tokenise (tokLst @ [TokBuiltInOp mathMap.[charstring [hd1;hd2]]],tl)
        | hd::tl when List.exists (fun el -> el = string hd) (keys opMap) -> tokenise (tokLst @ [TokSpecOp opMap.[string hd]],tl)
        //hd1::hd2 isn't needed for opMap since ARROWFUNC is taken care of by dashID, also hd1::hd2 might processes IF and IN, which we don't want
        | hd::tl when List.exists (fun el -> el = string hd) (keys spaceMap) -> tokenise (tokLst @ [TokWhitespace spaceMap.[string hd]],tl)
        | hd::tl ->
            match extractWord ([],otherLst) with //true, false, IF, LET, THEN, ELSE, IN, and LAMBDA only works when there is a space before and after
            | (boolTok,newOtherLst) when inMap boolMap boolTok -> tokenise (tokLst @ [boolMap.[boolTok] |> Bool |> TokLit],newOtherLst)
            | (mathTok,newOtherLst) when inMap mathMap mathTok -> tokenise (tokLst @ [mathMap.[mathTok] |> TokBuiltInOp],newOtherLst)
            | (spaceTok,newOtherLst) when inMap spaceMap spaceTok -> tokenise (tokLst @ [spaceMap.[spaceTok] |> TokWhitespace],newOtherLst)
            | (opTok,newOtherLst) when inMap opMap opTok -> tokenise  (tokLst @ [opMap.[opTok] |> TokSpecOp],newOtherLst)
            | ("not",newOtherLst) -> tokenise (tokLst @ [NOT |> TokUnaryOp],newOtherLst) // NEGATE is already processed via dashID function
            | (str,newOtherLst) -> tokenise (tokLst @ [str |> string |> TokIdentifier],newOtherLst)
        | [] -> tokLst
    tokenise ([],Seq.toList str)