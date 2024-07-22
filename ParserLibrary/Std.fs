// Higher-abstracted functions
module ParserLibrary.Std

open AST
open Core
open System

//// Character Based \\\\

// Matches a single character
let pchar charToMatch =
    let predicate c = c = charToMatch
    let label = $"{charToMatch}"
    satisfy predicate label

// Matches list of chars into parser
let anyOf chars =
    chars
    |> List.map pchar
    |> choice
    <?> $"any of {chars}"

// Convert char list to string
let charListToStr charList =
    charList |> List.toArray |> String
    
// Many char list to string
let manyChars c = many c |>> charListToStr
let many1Chars c = many1 c |>> charListToStr

// Whitespace
let wsChar = satisfy Char.IsWhiteSpace "ws"
let ws = many wsChar

// Specific characters
let parseLetter = ['a'..'z'] @ ['A'..'Z'] |> anyOf <?> "letter"
let digit = satisfy Char.IsDigit "digit"
let parseAlphanumeric = parseLetter <|> digit <?> "alphanumeric character"
let pquote = pchar '\'' <?> "quote"


//// String Based \\\\
let keyword str =
    str
    |> List.ofSeq
    |> List.map pchar
    |> sequence
    |>> charListToStr
    <?> str

let stringLit = between pquote (many parseAlphanumeric |>> charListToStr) pquote
                |>> StringLiteral
                <?> "string"

let identifier = many1Chars parseAlphanumeric <?> "identifier"


//// Number Based \\\\
let intLit =
    let resultToInt (sign, dlist) =
        let i = dlist |> int
        match sign with
        | Some _ -> -i
        | None -> i
         
    opt (pchar '-') .>>. many1Chars digit
    |>> resultToInt
    |>> IntLiteral
    <?> "int"
    
let floatLit =
    let resultToFloat (((sign, digits1), _), digits2) =
        let f = $"{digits1}.{digits2}" |> float
        match sign with
        | Some _ -> -f
        | None -> f
        
    opt (pchar '-') .>>. manyChars digit .>>. pchar '.' .>>. many1Chars digit
    |>> resultToFloat
    |>> FloatLiteral
    <?> "float"
        

//// Other Literals \\\\
let boolLit =
    keyword "true" <|> keyword "false"
    |>> bool.Parse
    |>> BoolLiteral
    <?> "bool"
    
let runeLit = between (pchar '`') parseAlphanumeric (pchar '`') |>> RuneLiteral
let voidLit = keyword "()"

let literal  = intLit <|> floatLit <|> stringLit <|> boolLit <|> runeLit |>> LiteralExpr
let explicitType = pchar ':' >>. ws >>.
                   (keyword "int" <|> keyword "float" <|> keyword "string" <|> keyword "bool" <|> keyword "rune") .>>
                   ws .>>. opt (keyword "array" <|> keyword "set")
                   <?> "explicit type"

let arrayLit = opt (keyword "set") .>> ws .>>. between (pchar '[')
                 (sepBy1 literal (pchar ',' .>> ws))
                 (pchar ']') <?> "array/set"
            
let mapLit = between (pchar '[')
                (sepBy1
                    (literal .>> ws .>> pchar ':' .>> ws .>>. literal)
                    (pchar ',' .>> ws))
                (pchar ']') <?> "map"
                
let recordLit = between (pchar '{')
                    (ws >>. opt (identifier .>> ws .>> keyword "with") .>> ws .>>.
                     (sepBy1 (identifier .>> ws .>> pchar '=' .>> ws .>>. literal .>> ws) (pchar ',' .>> ws)))
                    (pchar '}') <?> "record"
                    
let tupleLit = between (pchar '(')
                    (sepBy1 literal (pchar ',' .>> ws))
                    (pchar ')') <?> "tuple"
                    
let range = intLit .>> keyword ".." .>>. opt (intLit .>> keyword "..") .>>. intLit


//// Expressions \\\\
let rec accessExpr () = literal <|>
                        arrayExpr <|>
                        dataAccessExpr <|>
                        componentAccessExpr <|>
                        (identifier |>> IdentifierExpr)

and arrayExpr = identifier .>> ws .>>. between (pchar '[') (accessExpr()) (pchar ']') |>> ArrayExpr
and dataAccessExpr = identifier .>> pchar '.' .>>. identifier |>> DataAccessExpr
and componentAccessExpr = identifier .>> pchar '@' .>>. identifier |>> ComponentAccessExpr

let funcExpr = accessExpr() .>> ws .>>. sepBy1 (accessExpr()) ws |>> FunctionCallExpr <|> accessExpr()
let unaryExpr = (((pchar '!' <|> pchar '-') .>>. funcExpr) |>> UnaryExpr) <|> funcExpr


//// Bindings \\\\
let immutableBinding = keyword "let" >>. ws >>. identifier .>> ws .>>. opt explicitType .>> ws .>> pchar '=' .>> ws .>>. unaryExpr
let mutableBinding = keyword "var" >>. ws >>. identifier .>> ws .>>. opt explicitType .>> ws .>> pchar '=' .>> ws .>>. unaryExpr
let reassignment = identifier .>> ws .>> keyword "<-" .>> ws .>>. unaryExpr
let entityBinding = keyword "ent" >>. ws >>. identifier .>> ws .>> pchar '=' .>> ws .>>. sepBy1 identifier ws