// Core functions for parser composition
module ParserLibrary.Core

open ParserLibrary.InputState

type ParserLabel = string
type ParserError = string

type ParseResult<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserError * InputState

type Parser<'T> = {
    parseFunc : InputState -> ParseResult<'T * InputState>
    label : ParserLabel
}    

// Label and Error Messages
let printResult result =
    match result with
    | Success (value, _) -> printfn $"{value}"
    | Failure (label, error, parserPos) ->
        let col = parserPos.position.column
        let line = parserPos.position.line
        let caret = sprintf "%*s^ %s" col "" error
        printfn $"At line {line} col {col}: Error parsing {label}\n{currentLine parserPos}\n{caret}"

let setLabel parser newLabel =
    let createParser input =
        let result = parser.parseFunc input
        match result with
        | Success s -> Success s
        | Failure (_, err, pos) -> Failure (newLabel, err, pos)
    
    { parseFunc = createParser; label = newLabel }
let (<?>) = setLabel


let run parser input = parser.parseFunc input
let runString parser input = run parser (fromStr input)

let satisfy predicate label =
    let createParser input =
        let remaining, charOpt = nextChar input
        match charOpt with
        | None -> Failure (label, "No more input", input)
        | Some first ->
            if predicate first then Success (first, remaining)
            else Failure (label, $"Unexpected {first}", input)
    { parseFunc = createParser; label = label }


// Convert normal -> Parser to Parser -> Parser
let bind f p =
    let label = "unknown"
    let createParser input =
        let result = run p input
        match result with
        | Failure (label, err, pos) -> Failure (label, err, pos)
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

let rec parseZeroOrMore parser input =
    let result = run parser input
    
    match result with
    | Failure _ -> ([], input)
    | Success (first, rest) ->
        let subsequent, remaining = parseZeroOrMore parser rest
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
    parser >>= (fun head ->
    many parser >>= (fun tail ->
        returnP (head::tail)))
    <?> $"many1 {parser.label}"

// Match optionally (?)
let opt p =
    let some = p |>> Some
    let none = returnP None
    some <|> none

// AndThen with result ignoring
let (.>>) p1 p2 = p1 .>>. p2 |>> fst
let (>>.) p1 p2 = p1 .>>. p2 |>> snd
     
let between p1 p2 p3 = p1 >>. p2 .>> p3

let sepBy1 p sep =
    let sepThenP = sep >>. p
    p .>>. many sepThenP
    |>> fun (p, pList) -> p::pList
    <?> "separator"
let sepBy p sep =
    sepBy1 p sep <|> returnP []