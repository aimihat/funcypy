module Lexer
open Helpers

let intChars = ['0' .. '9']
let alphaNumeric = ['a' .. 'z'] @ ['A' .. 'Z'] @ intChars @ ['_']
let boolMap = Map ["true",true;"false",false]
let mathMap = Map ["+", Arithm Add; "-", Arithm Subtract;"*", Arithm Multiply;"/", Arithm Divide;"==",Comp Eq;"!=",Comp Ne;"<",Comp Lt;">",Comp Gt;"<=",Comp Le;">=",Comp Ge]
let spaceMap = Map ["\n",LineFeed]
let opMap = Map ["[",LSB;"]",RSB;"(",LRB;",",COMMA;"if",IF;"=",EQUALS;"def",DEF;":",COLON;"else",ELSE;")",RRB;"lambda",LAMBDA;"->",ARROWFUNC]
let unaryMap = Map ["not",NOT;"-",NEGATE]
let listFuncMap = Map ["isList" , ListF IsList ; "isEmpty", ListF IsEmpty ; "Head", ListF Head ; "Tail", ListF Tail]

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
            | hd::tl when hd = '>' -> (toks @ [TokSpecOp (ARROWFUNC)],tl) // ->: ARROWFUNC
            | hd::tl when hd = ' ' -> dashID (toks,tl) //ignore all spaces (except ARROWFUNC)
            | hd::tl when List.exists (fun el -> el = hd) intChars ->
                if tokLst <> [] then
                    match tokLst |> List.rev |> List.head with //check last token
                    | TokLit (Int _) -> (toks @ [TokBuiltInOp (Arithm Subtract)],others)
                    | TokLit (Double _) -> (toks @ [TokBuiltInOp (Arithm Subtract)],others) //#-#: SUBTRACT
                    | TokIdentifier _ -> (toks @ [TokBuiltInOp (Arithm Subtract)],others) //x-#: SUBTRACT
                    | _ -> buildNum (toks,others,false) //-#: negative #
                else buildNum (toks,others,false)
            | hd1::hd2::tl when hd1 = '.' && List.exists (fun el -> el = hd2) intChars -> //same as above, but for decimals
                if tokLst <> [] then
                    match tokLst |> List.rev |> List.head with
                    | TokLit (Int _) -> (toks @ [TokBuiltInOp (Arithm Subtract)],others)
                    | TokLit (Double _) -> (toks @ [TokBuiltInOp (Arithm Subtract)],others)
                    | TokIdentifier _ -> (toks @ [TokBuiltInOp (Arithm Subtract)],others)
                    | _ -> buildNum (toks,['0'] @ others,false)
                else buildNum (toks,['0'] @ others,false)
            | hd::tl when List.exists (fun el -> el = hd) alphaNumeric ->
                if tokLst <> [] then
                    match tokLst |> List.rev |> List.head with //check last token
                    | TokLit (Int _) -> (toks @ [TokBuiltInOp (Arithm Subtract)],others)
                    | TokLit (Double _) -> (toks @ [TokBuiltInOp (Arithm Subtract)],others) //#-x: SUBTRACT
                    | TokIdentifier _ -> (toks @ [TokBuiltInOp (Arithm Subtract)],others) //x-x: SUBTRACT
                    | _ -> (toks @ [TokUnaryOp (NEGATE)],others) //-x: negated variable - NEGATE
                else (toks @ [TokUnaryOp (NEGATE)],others)
            //the numbers are taken care of in the intChars match above, so this would only match for alphabets (identifiers or non-arithmetic operators): NEGATE
            | hd::tl when List.exists (fun el -> el = string hd) (keys mathMap) -> (toks @ [TokBuiltInOp (Arithm Subtract)],others) // --#: part of math expression - SUBTRACT
            | hd::tl when List.exists (fun el -> el = string hd) (keys opMap) -> (toks @ [TokBuiltInOp (Arithm Subtract)],others) // -(exp): part of math expression - SUBTRACT
            | [] -> (toks @ [TokBuiltInOp (Arithm Subtract)],[])
            //reached end of list: just send to SUBTRACT, the parse will fail in any way it's interpreted anyway
            //∵ for all cases ARROWFUNC, NEGATE, SUBTRACT and a negative #, a dash is meaningless if nothing comes after it
            | _ -> (toks @ [TokUnaryOp (NEGATE)],others) //for all other cases: NEGATE
        match otherLst with
        | hd::tl when hd = ' ' -> tokenise (tokLst,tl)
        | hd::tl when hd = '\t' -> tokenise (tokLst,tl)
        | hd::tl when hd = '\v' -> tokenise (tokLst,tl)
        | hd::tl when hd = '\r' -> tokenise (tokLst,tl)
        | hd::tl when hd = '\f' -> tokenise (tokLst,tl) // all whitespaces which should be ignored
        | hd::tl when hd = '-' -> tokenise (dashID (tokLst,tl)) // a '-' is found: identify ARROWFUNC, NEGATE, SUBTRACT or negative integer
        | hd::tl when hd = '\"' -> tokenise (buildString (tokLst,tl)) // a string is found
        | hd::tl when List.exists (fun el -> el = hd) intChars -> tokenise (buildNum (tokLst,otherLst,true))
        | hd1::hd2::tl when hd1 = '.' && List.exists (fun el -> el = hd2) intChars -> tokenise (buildNum (tokLst,['0'] @ otherLst,true)) //a decimal is found
        | hd::tl when List.exists (fun el -> el = string hd) (keys mathMap) -> tokenise (tokLst @ [TokBuiltInOp mathMap.[string hd]],tl)
        | hd1::hd2::tl when List.exists (fun el -> el = charstring [hd1;hd2]) (keys mathMap) -> tokenise (tokLst @ [TokBuiltInOp mathMap.[charstring [hd1;hd2]]],tl)
        | hd::tl when List.exists (fun el -> el = string hd) (keys opMap) -> tokenise (tokLst @ [TokSpecOp opMap.[string hd]],tl)
        //hd1::hd2 isn't needed for opMap since ARROWFUNC is taken care of by dashID, also hd1::hd2 might processes IF which we don't want
        | hd::tl when List.exists (fun el -> el = string hd) (keys spaceMap) -> tokenise (tokLst @ [TokWhitespace spaceMap.[string hd]],tl)
        | hd::tl ->
            match extractWord ([],otherLst) with //true, false, IF, LET, THEN, ELSE, IN, and LAMBDA only works when there is a space before and after
            | (boolTok,newOtherLst) when inMap boolMap boolTok -> tokenise (tokLst @ [boolMap.[boolTok] |> Bool |> TokLit],newOtherLst)
            | (mathTok,newOtherLst) when inMap mathMap mathTok -> tokenise (tokLst @ [mathMap.[mathTok] |> TokBuiltInOp],newOtherLst)
            | (spaceTok,newOtherLst) when inMap spaceMap spaceTok -> tokenise (tokLst @ [spaceMap.[spaceTok] |> TokWhitespace],newOtherLst)
            | (listFuncTok,newOtherLst) when inMap listFuncMap listFuncTok -> tokenise (tokLst @ [listFuncMap.[listFuncTok] |> TokBuiltInOp],newOtherLst)
            | (opTok,newOtherLst) when inMap opMap opTok -> tokenise  (tokLst @ [opMap.[opTok] |> TokSpecOp],newOtherLst)
            | ("not",newOtherLst) -> tokenise (tokLst @ [NOT |> TokUnaryOp],newOtherLst) // NEGATE is already processed via dashID function
            | (str,newOtherLst) -> tokenise (tokLst @ [str |> string |> TokIdentifier],newOtherLst)
        | [] -> tokLst
    tokenise ([],Seq.toList str)