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

    // Matches one parser then the other
    let andThen p1 p2 =
        let createParser input =
            let result1 = run p1 input

            match result1 with
            | Failure err -> Failure err
            | Success (value1, remaining1) ->
                let result2 = run p2 remaining1

                match result2 with
                | Failure err -> Failure err
                | Success (value2, remaining2) ->
                    let newValue = (value1, value2)
                    Success (newValue, remaining2)

        Parser createParser
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
    let map func parser =
        let createParser input =
            let result = run parser input
            
            match result with
            | Success (value, remaining) -> 
                Success (func value, remaining)
            | Failure err -> Failure err
        
        Parser createParser
    let (<!>) = map
    let (|>>) x f = map f x
    
    // Converts value to parser of value
    let returnP x = 
        let createParser input = Success(x, input)
        Parser createParser
    
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


// Higher-abstracted functions
open Core

let parseDigit = anyOf ['0'..'9']
let parseLetter = ['a'..'z'] @ ['A'..'Z'] |> anyOf
let parseAlphanumeric = parseLetter <|> parseDigit

let parse3Digits =
    (parseDigit .>>. parseDigit .>>. parseDigit)
    |>> fun ((c1, c2), c3) -> String [| c1; c2; c3; |]
    
let parse3DigitsInt = map int parse3Digits