module ParserLibrary

open System
    
// Line position and column tracking
module InputState =
    type Position = { line : int; column : int }
    type InputState = { lines: string array; position: Position }
    
    let initialPos = { line = 0; column = 0 }
    let incrCol pos = { pos with column = pos.column + 1 }
    let incrLine pos = { line = pos.line + 1; column = 0 }
    
    let fromStr str =
        if String.IsNullOrEmpty str then { lines = [||]; position = initialPos }
        else
            let separators = [| "\r\n"; "\n" |]
            let lines = str.Split(separators, StringSplitOptions.None)
            { lines = lines; position = initialPos }

// Core functions for parser composition
module Core =
    type ParserLabel = string
    type ParserError = string
    
    type ParseResult<'a> =
        | Success of 'a
        | Failure of ParserLabel * ParserError

    type Parser<'T> = {
        parseFunc : (string -> ParseResult<'T * string>)
        label : ParserLabel
    }    

    // Label and Error Messages
    let printResult result =
        match result with
        | Success (value, _) -> printfn $"{value}"
        | Failure (label, error) -> printfn $"Error parsing {label}: {error}"
    
    let setLabel parser newLabel =
        let createParser input =
            let result = parser.parseFunc input
            match result with
            | Success s -> Success s
            | Failure (oldLabel, err) -> Failure (newLabel, err)
        
        { parseFunc = createParser; label = newLabel }
    let (<?>) = setLabel
    
    
    let run (parser : Parser<_>) input =
        parser.parseFunc input

    let satisfy predicate label =
        let createParser input =
            if String.IsNullOrEmpty input then
                Failure (label, "No more input")
            else
                let first = input[0]
                if predicate first then
                    let remaining = input[1..]
                    Success (first, remaining)
                else
                    Failure (label, $"Unexpected {first}")
        { parseFunc = createParser; label = label }
    
    // Matches a single character
    let pchar charToMatch =
        let predicate c = c = charToMatch
        let label = $"{charToMatch}"
        satisfy predicate label

    
    // Convert normal -> Parser to Parser -> Parser
    let bind f p =
        let label = "unknown"
        let createParser input =
            let result = run p input
            match result with
            | Failure (label, err) -> Failure (label, err)
            | Success (value, remaining) ->
                let p2 = f value
                run p2 remaining
        { parseFunc = createParser; label = label }
    let (>>=) p f = bind f p
    
    // Converts value to parser of value
    let returnP x =
        let label = $"{x}"
        let createParser input = Success(x, input)
        { parseFunc = createParser; label = label }
    
    // Matches one parser then the other
    let andThen p1 p2 =
        p1 >>= (fun p1Result ->
        p2 >>= (fun p2Result ->
            returnP (p1Result, p2Result)))
        <?> $"{p1.label} and then {p2.label}"
        
    let (.>>.) = andThen

    // Matches one parser or the other
    let orElse p1 p2 =
        let label = $"{p1.label} or else {p2.label}"
        let createParser input =
            let result1 = run p1 input

            match result1 with
            | Success result -> Success result
            | Failure _ ->
                let result2 = run p2 input

                result2
            
        { parseFunc = createParser; label = label }
    let (<|>) = orElse

    // Matches one of any given parser
    let choice parsers = List.reduce (<|>) parsers

    // Matches list of chars into parser
    let anyOf chars =
        chars
        |> List.map pchar
        |> choice
        <?> $"any of {chars}"
    
    // Converts function to parser function
    let map func =
        bind (func >> returnP)
        
    let (<!>) = map
    let (|>>) x f = map f x
    
    // Separates parser of function to parser parametered function
    let apply fP xP =
        (fP .>>. xP)
        |>> fun (f,x) -> f x
    let (<*>) = apply

    // Various "Parser World" functions
    let lift2 f xP yP = returnP f <*> xP <*> yP
    let add = lift2 (+)
    let startsWith =
        let swNormal (str : string) (prefix : string) = str.StartsWith prefix 
        lift2 swNormal
    
    // Sequence of parsers
    let rec sequence parsers =
        let cons head tail = head::tail
        let consP = lift2 cons
        
        match parsers with
        | [] -> returnP []
        | head::tail ->
            consP head (sequence tail)

    let charListToStr charList =
        charList |> List.toArray |> String

    let rec parseZeroOrMore parser input =
        let result = run parser input
        
        match result with
        | Failure (label, err) -> ([], input)
        | Success (first, rest) ->
            let (subsequent, remaining) = parseZeroOrMore parser rest
            let values = first::subsequent
            (values, remaining)
    
    // Match zero or more (*)
    let many parser =
        let label = $"many {parser.label}"
        let createParser input =
            Success (parseZeroOrMore parser input)
        
        { parseFunc = createParser; label = label }
    
    // Match one or more (+)
    let many1 parser =
        let label = $"many1 {parser.label}"
        parser >>= (fun head ->
        many parser >>= (fun tail ->
            returnP (head::tail)))
    
    // Match optionally (?)
    let opt p =
        let some = p |>> Some
        let none = returnP None
        some <|> none

    // AndThen with result ignoring
    let (.>>) p1 p2 = p1 .>>. p2 |>> fun (a, b) -> a
    let (>>.) p1 p2 = p1 .>>. p2 |>> fun (a, b) -> b
         
    let between p1 p2 p3 = p1 >>. p2 .>> p3

    let sepBy1 p sep =
        let sepThenP = sep >>. p
        p .>>. many sepThenP
        |>> fun (p, pList) -> p::pList
    let sepBy p sep =
        sepBy1 p sep <|> returnP []


// Higher-abstracted functions
module Std = 
    open Core

    let whitespaceChar = satisfy Char.IsWhiteSpace "whitespace"
    let whitespace = many whitespaceChar

    let digit = satisfy Char.IsDigit "digit"
    let digits = many1 digit
    let parseLetter = ['a'..'z'] @ ['A'..'Z'] |> anyOf
    let parseAlphanumeric = parseLetter <|> digit

    let identifier str =
        str
        |> List.ofSeq
        |> List.map pchar
        |> sequence
        |>> charListToStr
        
    let pint =
        let resultToInt (sign, dlist) =
            let i = dlist |> List.toArray |> String |> int
            match sign with
            | Some _ -> -i
            | None -> i
            
        opt (pchar '-') .>>. digits
        |>> resultToInt
        
    let pquote = pchar '\''
    let pstring = between pquote (many parseAlphanumeric |>> charListToStr) pquote 

    let parray = between (pchar '[')
                     (sepBy1 pint (pchar ',' .>> whitespace))
                     (pchar ']')