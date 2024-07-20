module ParserLibrary

open System

// Core functions for parser composition
module Core = 
    type ParseResult<'a> =
        | Success of 'a
        | Failure of string

    type Parser<'T> = Parser of (string -> ParseResult<'T * string>)

    let run parser input =
        let (Parser func) = parser
        func input

    // Matches a single character
    let pchar charToMatch =
        let createParser str =
            if String.IsNullOrEmpty str then
                Failure "No more input"
            else
                if str[0] = charToMatch then
                    Success (charToMatch, str[1..])
                else
                    Failure $"Expecting {charToMatch}. Found {str[0]}"
        
        Parser createParser

    
    // Convert normal -> Parser to Parser -> Parser
    let bind f p =
        let createParser input =
            let result = run p input
            match result with
            | Failure err -> Failure err
            | Success (value, remaining) ->
                let p2 = f value
                run p2 remaining
        Parser createParser
    let (>>=) p f = bind f p
    
    // Converts value to parser of value
    let returnP x = 
        let createParser input = Success(x, input)
        Parser createParser
    
    // Matches one parser then the other
    let andThen p1 p2 =
        p1 >>= (fun p1Result ->
        p2 >>= (fun p2Result ->
            returnP (p1Result, p2Result)))
        
    let (.>>.) = andThen

    // Matches one parser or the other
    let orElse p1 p2 =
        let createParser input =
            let result1 = run p1 input

            match result1 with
            | Success result -> Success result
            | Failure _ ->
                let result2 = run p2 input

                result2
            
        Parser createParser
    let (<|>) = orElse

    // Matches one of any given parser
    let choice parsers = List.reduce (<|>) parsers

    // Matches list of chars into parser
    let anyOf chars =
        chars
        |> List.map pchar
        |> choice
    
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
        | Failure err -> ([], input)
        | Success (first, rest) ->
            let (subsequent, remaining) = parseZeroOrMore parser rest
            let values = first::subsequent
            (values, remaining)
    
    // Match zero or more (*)
    let many parser =
        let createParser input =
            Success (parseZeroOrMore parser input)
        
        Parser createParser
    
    // Match one or more (+)
    let many1 parser =
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
open Core

let whitespaceChar = anyOf [' '; '\t'; '\n']
let whitespace = many whitespaceChar

let parseDigit = anyOf ['0'..'9']
let digits = many1 parseDigit
let parseLetter = ['a'..'z'] @ ['A'..'Z'] |> anyOf
let parseAlphanumeric = parseLetter <|> parseDigit

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