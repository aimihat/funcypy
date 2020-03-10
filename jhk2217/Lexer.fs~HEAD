module Lexer

type BuiltInType = 
    | ADD 
    | SUBTRACT 
    | MULTIPLY 
    | DIVIDE
    | EQ 
    | NE 
    | LT 
    | GT 
    | LE 
    | GE

type Identifier = IDString of string

type Operator = 
    | LRB | IF | EQUALS | LET
    | THEN | ELSE | RRB | IN | LAMBDA | ARROWFUNC

type UnaryOps = NOT | NEGATE

type Literal = 
    | Bool of bool 
    | Int of int 
    | Double of double 
    | String of string 
    | Tuple of Literal*Literal

type Whitespace = 
    | Space             // ' '
    | FormFeed          // '\f'
    | LineFeed          // '\n'
    | CarriageReturn    // '\r'
    | HorizontalTab     // '\t'
    | VerticalTab       // '\v'

type Token = 
    | TokLit of Literal
    | TokUnaryOp of UnaryOps
    | TokSpecOp of Operator
    | TokIdentifier of Identifier
    | TokBuiltInOp of BuiltInType
    | TokWhitespace of Whitespace

let intChars = ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9']
let boolMap = Map ["true",true;"false",false]
let mathMap = Map ["+",ADD;"-",SUBTRACT;"*",MULTIPLY;"/",DIVIDE;"==",EQ;"!=",NE;"<",LT;">",GT;"<=",LE;">=",GE]
let spaceMap = Map [" ",Space;"\f",FormFeed;"\n",LineFeed;"\r",CarriageReturn;"\t",HorizontalTab;"\v",VerticalTab]
let opMap = Map ["(",LRB;"if",IF;"=",EQUALS;"let",LET;"then",THEN;"else",ELSE;")",RRB;"in",IN;"lambda",LAMBDA;"->",ARROWFUNC]
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
        | hd::tl when not (hd = ' ') -> extractWord (acc @ [hd],tl)
        | hd::tl -> (charstring acc,tl)
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
    let rec buildTuple (tokLst,otherLst) =
        //buildList: divide string into list separated by commas, but not the ones encapsulated by a string (two quotation marks)
        //make sure one division only contains one literal and nothing else, by the definition Tuple = Literal*Literal
        let rec buildList ((tok,str),lst) =
            let rec buildTupleStr (acc,lst,layer) =
                match lst with
                | hd::tl when hd = ']' && layer = 0 -> Ok (charstring acc,tl)
                | hd::tl when hd = ']' -> buildTupleStr (acc @ [hd],tl,layer-1)
                | hd::tl when hd = '[' -> buildTupleStr (acc @ [hd],tl,layer+1)
                | hd::tl -> buildTupleStr (acc @ [hd],tl,layer)
                | [] -> Error <| sprintf "The list has no ] bracket to end the list!"
            match lst with
            | hd::tl when (hd = ']') -> Some (tok @ [charstring str]), Ok tl
            | hd::tl when (hd = ',') -> buildList ((tok @ [charstring str],[]),tl)
            | hd::tl when (hd = ' ') -> buildList ((tok,str),tl) //For building tuples, ignore spaces
            | hd::tl when (hd = '\"') && str = [] ->
                match buildString ([],tl) with
                | (strTok,rest) when (skipSpaces rest).Head = ']' -> Some (tok @ ["\""+(strTok.Head |> (fun tok -> match tok with TokLit (String builtStr) -> builtStr))]), Ok (skipSpaces rest).Tail
                | (strTok,rest) when (skipSpaces rest).Head = ',' -> buildList ((tok @ ["\""+(strTok.Head |> (fun tok -> match tok with TokLit (String builtStr) -> builtStr))],[]),(skipSpaces rest).Tail)
                | _ -> None, Error <| sprintf "The list is not parsable!" //the list element has something besides the one string literal
            | hd::tl when (hd = '[') ->
                match buildTupleStr ([],tl,0) with
                | Ok (tupleStr,rest) when (skipSpaces rest).Head = ']' -> Some (tok @ ["["+tupleStr+"]"]), Ok (skipSpaces rest).Tail
                | Ok (tupleStr,rest) when (skipSpaces rest).Head = ',' -> buildList ((tok @ ["["+tupleStr+"]"],[]),(skipSpaces rest).Tail)
                | _ -> None, Error <| sprintf "The list is not parsable!" //the nested list element has something besides the one string literal
            | hd::tl -> buildList ((tok,str @ [hd]),tl)
            | [] -> Some (tok), Ok lst //This should never occur since there is guaranteed to be a RSB in lst due to the if-then-else statement
        let strToLit str =
            match str with
            | boolLit when inMap boolMap boolLit -> boolMap.[boolLit] |> Bool |> Ok
            | _ ->
                match Seq.toList str with
                | hd::tl when List.exists (fun el -> el = hd) intChars ->
                    match buildNum([],hd::tl,true) with
                    | (numTok,[]) -> numTok |> List.head |> (fun tok -> match tok with |TokLit numLit -> Ok numLit)
                    | _ -> Error <| sprintf "%s is not a literal!" str
                | hd::tl when hd = '-' ->
                    match buildNum([],tl,false) with
                    | (numTok,[]) -> numTok |> List.head |> (fun tok -> match tok with |TokLit numLit -> Ok numLit)
                    | _ -> Error <| sprintf "%s is not a literal!" str
                | hd::tl when hd = '\"' ->
                    tl |> charstring |> String |> Ok
                    //format was checked via match buildString: can immediately format into string literal 
                | hd::tl when hd = '[' ->
                    match buildTuple([],tl) with
                    | (tupleTok,[]) -> tupleTok |> List.head |> (fun tok -> match tok with |TokLit tupleLit -> Ok tupleLit)
                    | _ -> Error <| sprintf "%s is not a literal!" str
                | [] -> String "" |> Ok //for empty list
                | _ -> Error <| sprintf "%s is not a literal!" str
        let lstToTup strLst =
            match strLst |> List.map strToLit |> List.tryFind (fun el -> match el with |Ok _ -> false|Error _ -> true) with
            | Some error -> error |> (fun el -> match el with Error msg -> msg) |> failwithf "%A"
            | None when (List.length strLst) = 0 -> Tuple(String "",String "")
            | None when (List.length strLst) = 1 -> strLst |> List.map strToLit |> List.map (fun el -> match el with Ok lit -> lit) |> List.fold (fun a b -> Tuple(a,b)) (String "")
            | None -> strLst |> List.map strToLit |> List.map (fun el -> match el with Ok lit -> lit) |> List.reduce (fun a b -> Tuple(a,b))
        let tupleOpt (toks,others) =
            if (List.exists(fun el -> el = ']') others)
            then
                match buildList (([],[]),others) with
                | Some strLst, Ok newOthers -> Some (toks @ [TokLit (strLst |> lstToTup)]), Ok newOthers
                | _,  Error msg -> failwithf "%A" msg //This should never occur since lstToTup fails all instances of buildList that would fail compilation
            else None, Error <| sprintf "The list has no ] bracket to end the list!"
        match tupleOpt (tokLst,otherLst) with
        | Some newTokLst, Ok newOtherLst -> newTokLst,newOtherLst
        | None, Ok newOtherLst -> (tokLst @ [TokLit (Tuple (String "",String ""))],newOtherLst)//This should never occur since newTokLst is at the very least the existing list tokLst
        | _, Error msg -> failwithf "%A" msg
    let rec tokenise (tokLst,otherLst) =
        let rec dashID (toks,others) = //must figure out if '-' is negative sign, NEGATE or SUBTRACT
            match others with
            | hd::tl when hd = '>' -> (toks @ [TokSpecOp (ARROWFUNC)],tl) // ->
            | hd::tl when List.exists (fun el -> el = hd) intChars ->
                match tokLst |> List.rev |> List.head with // #-#
                | TokLit (Int _) -> (toks @ [TokBuiltInOp (SUBTRACT)],others)
                | TokLit (Double _) -> (toks @ [TokBuiltInOp (SUBTRACT)],others)
                | _ -> buildNum (toks,others,false)
            | hd::tl when hd = ' ' ->
                match skipSpaces tl  with
                | hd::tl when List.exists (fun el -> el = hd) intChars -> (toks @ [TokBuiltInOp (SUBTRACT)],others) // - #
                | '-'::hd::tl when List.exists (fun el -> el = hd) intChars -> (toks @ [TokBuiltInOp (SUBTRACT)],others) // - -#
                | hd::tl -> (toks @ [TokUnaryOp (NEGATE)],others)
                | [] -> //reached end of string
                    match tokLst |> List.rev |> skipSpaceToks |> List.head with // # -
                    | TokLit (Int _) -> (toks @ [TokBuiltInOp (SUBTRACT)],others)
                    | TokLit (Double _) -> (toks @ [TokBuiltInOp (SUBTRACT)],others)
                    | TokBuiltInOp _ -> (toks @ [TokBuiltInOp (SUBTRACT)],others)
                    | _ -> (toks @ [TokUnaryOp (NEGATE)],others)
            | hd::tl when inMap mathMap (string hd) -> // - +
                match tokLst |> List.rev |> List.head with // #-#
                | TokLit (Int _) -> (toks @ [TokBuiltInOp (SUBTRACT)],others)
                | TokLit (Double _) -> (toks @ [TokBuiltInOp (SUBTRACT)],others)
                | _ -> buildNum (toks,others,false)
            | hd1::hd2::tl when List.exists (fun el -> el = charstring [hd1;hd2]) (keys mathMap) -> // - ==
                match tokLst |> List.rev |> List.head with // #-#
                | TokLit (Int _) -> (toks @ [TokBuiltInOp (SUBTRACT)],others)
                | TokLit (Double _) -> (toks @ [TokBuiltInOp (SUBTRACT)],others)
                | _ -> buildNum (toks,others,false)
            | [] -> //reached end of string
                match tokLst |> List.rev |> skipSpaceToks |> List.head with // # -
                | TokLit (Int _) -> (toks @ [TokBuiltInOp (SUBTRACT)],others)
                | TokLit (Double _) -> (toks @ [TokBuiltInOp (SUBTRACT)],others)
                | TokBuiltInOp _ -> (toks @ [TokBuiltInOp (SUBTRACT)],others)
                | _ -> (toks @ [TokUnaryOp (NEGATE)],others)
            | _ -> (toks @ [TokUnaryOp (NEGATE)],others)
        match otherLst with
        | hd::tl when hd = ' ' -> tokenise (tokLst @ [TokWhitespace Space],tl)
        | hd::tl when hd = '-' -> tokenise (dashID (tokLst,tl))
        | hd::tl when hd = '\"' -> tokenise (buildString (tokLst,tl))
        | hd::tl when hd = '[' -> tokenise (buildTuple (tokLst,tl))
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
            | (str,newOtherLst) -> tokenise (tokLst @ [str |> IDString |> TokIdentifier],newOtherLst)
        | [] -> tokLst
    tokenise ([],Seq.toList str)