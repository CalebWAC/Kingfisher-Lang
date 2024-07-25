// Higher-abstracted functions
module ParserLibrary.Std

open System.Diagnostics
open AST
open Core
open System

let stack = StackTrace(true)

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
let ws = many wsChar <?> "whitespace"
let ws1 = many1 wsChar <?> "whitespace"


// Specific characters
let parseLetter = ['a'..'z'] @ ['A'..'Z'] |> anyOf <?> "letter"
let digit = satisfy Char.IsDigit "digit"
let parseAlphanumeric = parseLetter <|> digit <?> "alphanumeric character"
let parseAny = parseAlphanumeric <|> wsChar <|> anyOf [':'; '$'; '{'; '}'; '.'; '?'; '!'; '@'; '#'; '$'; '%'; '^'; '&'; '*'; '('; ')']
let pquote = pchar '\'' <?> "quote"


//// String Based \\\\
let keyword str =
    str
    |> List.ofSeq
    |> List.map pchar
    |> sequence
    |>> charListToStr
    <?> str

let stringLit = between pquote (many parseAny |>> charListToStr) pquote
                |>> StringLiteral
                <?> "string"

let identifier = parseLetter .>>. manyChars parseAlphanumeric
                 |>> fun (c, s) -> c.ToString() + s
                 >>= fun s -> if List.contains s ["true"; "false"; "do"; "while"; "if"; "let"; "var"; "for"; "in"; "then"; "elif"; "else"; "type"; "fun"; "ent"; "com"; "sys"; "when"; "is"; "where"; "impl"] |> not then
                                returnP s
                              else
                                  let createParser input = Failure("identifier", "err", input)
                                  { parseFunc = createParser; label = "" }
                 <?> "identifier"


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

let explicitType = pchar ':' >>. ws >>.
                   (keyword "int" <|> keyword "float" <|> keyword "string" <|> keyword "bool" <|> keyword "rune") .>>
                   ws .>>. opt (keyword "array" <|> keyword "set")
                   |>> AST.Type
                   <?> "explicit type"

let rec literal () = floatLit <|> intLit <|> stringLit <|> boolLit <|> runeLit <|>
                     (arrayLit |>> CollectionLiteral) <|> (mapLit |>> CollectionLiteral) <|> recordLit <|> tupleLit |>> LiteralExpr

and arrayLit = opt (keyword "set") .>> ws .>>. between (pchar '[')
                 (sepBy1 (literal()) (pchar ',' .>> ws))
                 (pchar ']') |>> ArrayLiteral <?> "array/set" 
            
and mapLit = between (pchar '[')
                (sepBy1
                    (literal() .>> ws .>> pchar ':' .>> ws .>>. literal())
                    (pchar ',' .>> ws))
                (pchar ']') |>> MapLiteral <?> "map"
                
and recordLit = between (pchar '{')
                    (ws >>. opt (identifier .>> ws .>> keyword "with") .>> ws .>>.
                     (sepBy1 (identifier .>> ws .>> pchar '=' .>> ws .>>. literal() .>> ws) (pchar ',' .>> ws)))
                    (pchar '}') |>> RecordLiteral <?> "record"
                    
and tupleLit = between (pchar '(')
                    (sepBy1 (literal()) (pchar ',' .>> ws))
                    (pchar ')') |>> TupleLiteral <?> "tuple"
                    
and range = intLit .>> keyword ".." .>>. opt (intLit .>> keyword "..") .>>. intLit <?> "range" 


//// Operators \\\\
let unaryOp =
    pchar '!' <|> pchar '-'
    |>> fun char -> if char = '!' then Not else Negative

let binaryArithOp =
    pchar '+' <|> pchar '-' <|> pchar '*' <|> pchar '/' <|> pchar '%' <|> pchar '^'
    |>> fun char -> match char with
                    | '+' -> Add
                    | '-' -> Sub
                    | '*' -> Mul
                    | '/' -> Div
                    | '%' -> Mod
                    |  _  -> Exp
                    
let binaryCompOp =
    keyword "=" <|> keyword "!=" <|> keyword ">=" <|> keyword "<=" <|> keyword ">" <|> keyword "<"
    |>> fun str -> match str with
                   | "="  -> Equal
                   | "!=" -> NotEqual
                   | ">=" -> GreaterEqual
                   | "<=" -> LessEqual
                   | ">"  -> GreaterThan
                   |  _   -> LessThan
                   
let binaryLogOp =
    keyword "&&" <|> keyword "||"
    |>> fun str -> if str = "&&" then And else Or


//// Expressions \\\\
let rec expr () = ((binaryCompExpr .>> ws .>>. binaryLogOp .>> ws .>>. binaryCompExpr) |>> BinaryLogicalExpr <|> binaryCompExpr) //<?> "expression"


and arrayExpr = identifier .>> ws .>>. between (pchar '[') (literal() <|> (identifier |>> IdentifierExpr)) (pchar ']') |>> ArrayExpr <?> "array access"
and dataAccessExpr = identifier .>> pchar '.' .>>. identifier |>> DataAccessExpr <?> "data access"
and componentAccessExpr = identifier .>> pchar '@' .>>. identifier |>> ComponentAccessExpr <?> "component access"
and accessExpr = literal() <|>
                 arrayExpr <|>
                 dataAccessExpr <|>
                 componentAccessExpr <|>
                 (identifier |>> IdentifierExpr) <?> "access expr"

and funcExpr = identifier .>> ws1 .>>. sepBy1 accessExpr ws1 |>> FunctionCallExpr <|> accessExpr <?> "function call"
and unaryExpr = (unaryOp .>>. funcExpr) |>> UnaryExpr <|> funcExpr <?> "unary expression"
and binaryArithExpr = (unaryExpr .>> ws .>>. binaryArithOp .>> ws .>>. unaryExpr) |>> BinaryArithmeticExpr <|> unaryExpr <?> "binaryArth expr"
and binaryCompExpr = (binaryArithExpr .>> ws .>>. binaryCompOp .>> ws .>>. binaryArithExpr) |>> BinaryComparisonExpr <|> binaryArithExpr <?> "binaryComp expr"


let rec statement () = (binding <|> (expression |>> Statement.Expression)) <?> "statement"


//// Control Flow Expressions \\\\
and expressionFor() = (expr() |>> Expression) <|> ifExpr() <|> whileExpr() <|> matchExpr() |>> Statement.Expression //<|> forExpr()
and expressionWhile() = (expr() |>> Expression) // <|> ifExpr() <|> forExpr() <|> matchExpr()  // <|> whileExpr()
and expressionIf() = (expr() |>> Expression) // <|> forExpr() <|> whileExpr() <|> matchExpr() // <|> ifExpr()
and expressionMatch() = (expr() |>> Expression) // <|> ifExpr() <|> forExpr() <|> whileExpr() // <|> matchExpr()

and ifCond = (expr() |>> IfCondition.Expr) <|> (keyword "let" >>. ws >>. identifier .>> ws .>> pchar '=' .>>. expr() |>> LetStatement)
and ifExpress() = ifCond .>> ws .>>. opt (keyword "where" >>. ws >>. expr()) .>> ws .>> keyword "then" .>> ws .>> keyword "{" .>> ws .>>. many1 (expr() .>> ws) .>> pchar '}' |>> IfExpress
and ifExpr() = keyword "if" >>. ws >>. ifExpress() .>> ws .>>. opt(many1 (keyword "elif" >>. ifExpress())) .>> ws .>>. opt (keyword "else" >>. ws >>. pchar '{' >>. ws >>. many1 (expr() .>> ws) .>> pchar '}') |>> IfExpr  <?> "if"

and forExpr() = opt (identifier .>> pchar '@') .>> ws .>> keyword "for" .>> ws .>>. identifier .>> ws .>> keyword "in" .>> ws .>>. expr() .>> ws .>>. opt (keyword "where" >>. ws >>. expr()) .>> ws .>> keyword "do" .>> ws .>> pchar '{' .>> ws .>>. many1 (expressionFor() .>> ws) .>> pchar '}' |>> ForExpr <?> "for loop"
and whileExpr() = keyword "while" >>. ws >>. expr() .>> ws .>> keyword "do" .>> ws .>> pchar '{' .>> ws .>>. many1 (expressionWhile() .>> ws) .>> pchar '}' |>> WhileExpr <?> "while loop"
and matchExpr() = keyword "when" >>. ws >>. identifier .>> ws .>> keyword "is" .>> ws .>> pchar '{' .>> ws .>>. many1 (identifier .>> ws .>> keyword "->" .>> ws .>>. expressionMatch() .>> ws) .>> ws .>> pchar '}' |>> MatchExpr <?> "match"

and expression = ifExpr() <|> forExpr() <|> whileExpr() <|> matchExpr() <|> (expr() |>> Expression) <?> "higher expression"


//// Bindings \\\\
and immutableBinding = keyword "let" >>. ws >>. identifier .>> ws .>>. opt explicitType .>> ws .>> pchar '=' .>> ws .>>. expr() |>> ImmutableBinding
and mutableBinding = keyword "var" >>. ws >>. identifier .>> ws .>>. opt explicitType .>> ws .>> pchar '=' .>> ws .>>. expr() |>> MutableBinding
and reassignment = identifier .>> ws .>> keyword "<-" .>> ws .>>. expr() |>> Reassignment

and entityBinding = keyword "ent" >>. ws >>. identifier .>> ws .>> pchar '=' .>> ws .>>. sepBy1 identifier ws1 |>> EntityBinding
and systemBinding = keyword "sys" >>. ws >>. sepBy1 identifier ws .>> ws .>>. opt (pchar '|' >>. ws >>. identifier .>> ws) .>> pchar '=' .>> ws .>> pchar '{' .>> ws .>>. many1 expression .>> ws .>> pchar '}' |>> SystemDeclaration

and parameter = (pchar '(' >>. ws >>. ((opt identifier .>> ws .>>. identifier) <|> (opt (keyword "~") .>>. identifier)) .>> ws .>>. opt explicitType .>> ws .>> pchar ')' |>> Specified) <|> (identifier |>> Unspecified) .>> ws <?> "parameter"
and functionBinding = keyword "fun" >>. ws >>. identifier .>> ws .>>. many parameter .>> ws .>>. opt explicitType .>> ws .>> pchar '=' .>> ws .>> pchar '{' .>> ws .>>. many1 (expr()) .>> ws .>> pchar '}' |>> FunctionDeclaration

and binding = (immutableBinding <|> mutableBinding <|> entityBinding <|> functionBinding <|> systemBinding <|> reassignment) |>> Binding <?> "binding"


let parseProgram = ws >>. sepBy1 (statement()) ws