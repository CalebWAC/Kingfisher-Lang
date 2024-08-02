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
let ws1NoNl = many1 (satisfy (fun c -> c = ' ' || c = '\t') "ws")


// Specific characters
let parseLetter = ['a'..'z'] @ ['A'..'Z'] |> anyOf <?> "letter"
let digit = satisfy Char.IsDigit "digit"
let parseAlphanumeric = parseLetter <|> digit <?> "alphanumeric character"
let parseAny = parseAlphanumeric <|> wsChar <|> anyOf [':'; '$'; '{'; '}'; '.'; '?'; '!'; '@'; '#'; '$'; '%'; '^'; '&'; '*'; '('; ')'; ','; '<'; '>'; '/']
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
let decLit =
    let resultToInt ((sign, dlist), unsigned : char option) =
        let i = dlist |> int
        match sign with
        | Some _ -> if unsigned.IsSome then Exception "uint cannot be negative" |> raise else -i
        | None -> i
         
    opt (pchar '-') .>>. many1Chars digit .>>. opt (pchar 'u')
    |>> resultToInt
    |>> IntLiteral
    <?> "int"
   
let hexLit =
    let resultToInt (sign, dlist) =
        let i = Convert.ToInt32(dlist, 16)
        match sign with
        | Some _ -> -i
        | None -> i
    
    opt (pchar '-') .>> keyword "0x" .>>. many1Chars digit
    |>> resultToInt
    |>> IntLiteral
    <?> "hex literal"

let binLit =
    let resultToInt (sign, dlist) =
        let i = Convert.ToInt32(dlist, 2)
        match sign with
        | Some _ -> -i
        | None -> i
        
    opt (pchar '-') .>> keyword "0b" .>>. many1Chars digit
    |>> resultToInt
    |>> IntLiteral
    <?> "bin literal"
    
let octLit =
    let resultToInt (sign, dlist) =
        let i = Convert.ToInt32(dlist, 8)
        match sign with
        | Some _ -> -i
        | None -> i
        
    opt (pchar '-') .>> keyword "0o" .>>. many1Chars digit
    |>> resultToInt
    |>> IntLiteral
    <?> "oct literal"
    
let intLit = binLit <|> octLit <|> hexLit <|> decLit
    
let floatLit =
    let resultToFloat ((((sign, digits1), _), digits2), dec) =
        let f = $"{digits1}.{digits2}" |> double
        match sign with
        | Some _ -> (-f, dec)
        | None -> (f, dec)
        
    opt (pchar '-') .>>. manyChars digit .>>. pchar '.' .>>. many1Chars digit .>>. opt (pchar 'd')
    |>> resultToFloat
    |>> (fun (f, dec) -> if dec.IsNone then f |> float |> FloatLiteral else f |> DoubleLiteral)
    <?> "float"
        

//// Other Literals \\\\
let boolLit =
    keyword "true" <|> keyword "false"
    |>> bool.Parse
    |>> BoolLiteral
    <?> "bool"
    
let runeLit = between (pchar '`') parseAlphanumeric (pchar '`') |>> RuneLiteral <?> "rune"
let voidLit = keyword "()" |>> VoidLiteral <?> "void"


let range, rangeRef = parserToRef()

let typeKeyWord = identifier .>>. opt (pchar '?') .>> ws .>>. opt (keyword "array" <|> keyword "set")
                   |>> fun ((str, option), optCol) ->
                           let typeKey = match str with
                                         | "int" -> Int
                                         | "float" -> Float
                                         | "string" -> TypeKeyWord.String
                                         | "bool" -> Bool
                                         | "rune" -> Rune
                                         | s -> Custom s
                           let colType = match optCol with
                                         | Some col -> if col = "array" then Some CollectionType.Array else Some Set
                                         | None -> None
                           AST.Type ((if option.IsSome then Option typeKey else typeKey), colType)
let explicitType = pchar ':' >>. ws >>. typeKeyWord <?> "explicit type"


let arrayLit, arrayLitRef = parserToRef()
            
let mapLit, mapLitRef = parserToRef() 
                    
let recordLit, recordLitRef = parserToRef()
                    
let tupleNameLit = identifier .>> ws .>> pchar ',' .>> ws .>>. identifier

let literal () = floatLit <|> intLit <|> stringLit <|> boolLit <|> runeLit <|>
                 (arrayLit |>> CollectionLiteral) <|> (mapLit |>> CollectionLiteral) <|> voidLit
                 |>> LiteralExpr <?> "literal"



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
let parenExpr, parenExprRef = parserToRef()
let arrayExpr, arrayExprRef = parserToRef()
let dataAccessExpr, dataExRef = parserToRef()
let componentAccessExpr = identifier .>> pchar '@' .>>. identifier |>> ComponentAccessExpr <?> "component access"
let accessExpr = parenExpr <|>
                 range <|>
                 literal() <|>
                 (recordLit |>> LiteralExpr) <|>
                 arrayExpr <|>
                 dataAccessExpr <|>
                 componentAccessExpr <|>
                 (identifier |>> IdentifierExpr) <?> "access expr"

let funcExpr() = identifier .>> ws1NoNl .>>. sepBy1 (opt (identifier .>> pchar ':') .>> ws .>>. accessExpr) ws1NoNl |>> FunctionCallExpr <|> accessExpr <?> "function call"
let unaryExpr = (unaryOp .>>. funcExpr()) |>> UnaryExpr <|> funcExpr() <?> "unary expression"

let binaryArithExpr = (unaryExpr .>>. many1 (ws >>. binaryArithOp .>> ws .>>. unaryExpr)) |>> BinaryArithmeticExpr <|> unaryExpr <?> "binaryArth expr"
let binaryCompExpr = (binaryArithExpr .>> ws .>>. binaryCompOp .>> ws .>>. binaryArithExpr) |>> BinaryComparisonExpr <|> binaryArithExpr <?> "binaryComp expr"

let expr = ((binaryCompExpr .>>. many1 (ws >>. binaryLogOp .>> ws .>>. binaryCompExpr)) |>> BinaryLogicalExpr <|> binaryCompExpr) <?> "expression"

// Resolving references
recordLitRef.Value <- between (pchar '{')
                        (ws >>. opt (identifier .>> ws .>> keyword "with") .>> ws .>>.
                         (sepBy1 (identifier .>> ws .>> pchar '=' .>> ws .>>. expr .>> ws) (pchar ',' .>> ws)))
                        (pchar '}') |>> RecordLiteral <?> "record"
                        
arrayLitRef.Value <- opt (keyword "set") .>> ws .>>. between (pchar '[')
                     (sepBy1 expr (pchar ',' .>> ws)) (pchar ']')
                     |>> fun (strOpt, col) -> if strOpt.IsSome then ArrayLiteral(Set, col) else ArrayLiteral(CollectionType.Array, col)
                     <?> "array/set"
                     
mapLitRef.Value <- between (pchar '[' .>> ws)
                    (sepBy1
                        (expr .>> ws .>> pchar ':' .>> ws .>>. expr)
                        (pchar ',' .>> ws))
                    (ws .>> pchar ']') |>> MapLiteral <?> "map"
                    
arrayExprRef.Value <- identifier .>> ws .>>. between (pchar '[') expr (pchar ']') |>> ArrayExpr <?> "array access"
dataExRef.Value <- identifier .>> pchar '.' .>>. expr |>> DataAccessExpr <?> "data access"
parenExprRef.Value <- between (pchar '(') (ws >>. expr .>> ws) (pchar ')') |>> Parenthesis

let rangable = literal() <|> arrayExpr <|> dataAccessExpr <|> componentAccessExpr <|> (identifier |>> IdentifierExpr)
rangeRef.Value <- rangable .>>. (keyword "..." <|> keyword "..") .>>. opt (rangable .>>. (keyword "..." <|> keyword "..")) .>>. rangable
                  |>> fun (((i1, r1), step), i2) ->
                      let i1 = i1
                      let i2 = i2 
                      if step.IsSome then
                          if snd step.Value = ".." then RangeExpr ((i1, ExclusiveStep (fst step.Value)), i2)
                          else RangeExpr ((i1, InclusiveStep (fst step.Value)), i2)
                      else
                          if r1 = ".." then RangeExpr ((i1, Exclusive), i2)
                          else RangeExpr ((i1, Inclusive), i2)
                 <?> "range" 


let statement, binding, expression, typeDeclaration =
    //// Bindings \\\\
    let immutableBinding = keyword "let" >>. ws >>. identifier .>> ws .>>. opt explicitType .>> ws .>> pchar '=' .>> ws .>>. expr |>> ImmutableBinding
    let mutableBinding = keyword "var" >>. ws >>. identifier .>> ws .>>. opt explicitType .>> ws .>> pchar '=' .>> ws .>>. expr |>> MutableBinding
    let reassignment = (dataAccessExpr <|> (identifier |>> IdentifierExpr)) .>> ws .>> keyword "<-" .>>. opt binaryArithOp .>> ws .>>. expr |>> Reassignment

    let entityBinding = keyword "ent" >>. ws >>. identifier .>> ws .>> pchar '=' .>> ws .>>. sepBy1 (identifier .>> ws .>>. opt expr) ws1 |>> EntityBinding
    
    let parameter = (pchar '(' >>. ws >>. ((opt identifier .>> ws .>>. identifier) <|> (opt (keyword "~") .>>. identifier)) .>> ws .>>. opt explicitType .>> ws .>> pchar ')' |>> Specified) <|> (identifier |>> Unspecified) .>> ws <?> "parameter"
    let functionBinding, funcBindRef = parserToRef()         

    let binding = choice [immutableBinding; mutableBinding; entityBinding; functionBinding; reassignment] |>> Binding <?> "binding"

    let ifCond = (expr |>> IfCondition.Expr) <|> (keyword "let" >>. ws >>. identifier |>> IfCondition.LetStatement)
    let ifExpress, ifExpressRef = parserToRef() 
    let ifExpr, ifExprRef = parserToRef() 

    let forExpr, forExprRef = parserToRef() 
    let whileExpr, whileExprRef = parserToRef()
    let matchExpr, matchExprRef = parserToRef()

    let expression = choice [ ifExpr; forExpr; whileExpr; matchExpr; expr |>> Expression ] <?> "higher expression"

    
    // Type Declarations
    let unionDeclaration = keyword "type" >>. ws >>. identifier .>> ws .>> keyword ":=" .>> ws .>>. many1 (pchar '|' >>. ws >>. identifier .>> ws .>>. opt (keyword "of" >>. ws >>. typeKeyWord .>> ws .>>. many (pchar '*' >>. ws >>. typeKeyWord .>> ws)))
                           |>> fun (name, cases) ->
                                   let ucases = [
                                       for c in cases do
                                           if (snd c).IsSome then yield Multiple (fst c, (fst (snd c).Value)::(snd (snd c).Value))
                                           else yield UnionCase.Single (fst c)
                                   ]
                                   UnionDeclaration (name, ucases)
                           <?> "unionDeclaration"
    
    let recordDeclaration = keyword "type" >>. ws >>. identifier .>> ws .>> keyword "=" .>> ws .>> pchar '{' .>>. sepBy1 (ws >>. opt (keyword "var") .>> ws .>>. identifier .>> ws .>>. explicitType) (pchar ',') .>> ws .>> pchar '}' |>> RecordDeclaration       
    
    let componentDeclaration = keyword "com" >>. ws >>. identifier .>> ws .>> keyword ":=" .>> ws .>> pchar '{' .>> ws .>>. many1 (identifier .>> ws .>>. explicitType) .>> ws .>> pchar '}' |>> ComponentDeclaration
    
    let systemBinding, sysBindRef = parserToRef()
   
    let extension, extensionRef = parserToRef()
    
    let typeDeclaration = choice [ unionDeclaration; recordDeclaration; componentDeclaration; systemBinding; extension ] |>> TypeDeclaration
    
    
    let statement = choice [ binding; expression |>> Statement.Expression; typeDeclaration ] <?> "statement"
    
    
    // Fixing references
    funcBindRef.Value <- keyword "fun" >>. ws >>. identifier .>> ws .>>. many parameter .>> ws .>>. opt explicitType .>> ws .>> keyword ":=" .>> ws .>> pchar '{' .>> ws .>>. many1 (statement .>> ws) .>> ws .>> pchar '}' |>> FunctionDeclaration
    ifExpressRef.Value <- ifCond .>> ws .>>. opt (keyword "where" >>. ws >>. expr) .>> ws .>> keyword "then" .>> ws .>> keyword "{" .>> ws .>>. many1 (statement .>> ws) .>> pchar '}' |>> IfExpress
    ifExprRef.Value <- keyword "if" >>. ws >>. ifExpress .>> ws .>>. opt(many1 (keyword "elif" >>. ws >>. ifExpress .>> ws)) .>> ws .>>. opt (keyword "else" >>. ws >>. pchar '{' >>. ws >>. many1 (statement .>> ws) .>> pchar '}') |>> IfExpr <?> "if"
    forExprRef.Value <- opt (identifier .>> pchar '@') .>> ws .>> keyword "for" .>> ws .>>. (tupleNameLit |>> MapDestructuring <|> (identifier |>> Identifier)).>> ws .>> keyword "in" .>> ws .>>. expr .>> ws .>>. opt (keyword "where" >>. ws >>. expr) .>> ws .>> keyword "do" .>> ws .>> pchar '{' .>> ws .>>. many1 (statement .>> ws) .>> pchar '}' |>> ForExpr <?> "for loop"
    whileExprRef.Value <- keyword "while" >>. ws >>. expr .>> ws .>> keyword "do" .>> ws .>> pchar '{' .>> ws .>>. many1 (statement .>> ws) .>> pchar '}' |>> WhileExpr <?> "while loop"
    matchExprRef.Value <- keyword "when" >>. ws >>. identifier .>> ws .>> keyword "is" .>> ws .>> pchar '{' .>> ws .>>. many1 (expr .>> ws .>> keyword "->" .>> ws .>>. many1 (statement .>> ws)) .>> ws .>> pchar '}' |>> MatchExpr <?> "match"
    
    sysBindRef.Value <- keyword "sys" >>. ws >>. sepBy1 identifier ws .>> ws .>>. opt (pchar '|' >>. ws >>. identifier .>> ws) .>> keyword ":=" .>> ws .>> pchar '{' .>> ws .>>. many1 (statement .>> ws) .>> ws .>> pchar '}'
                        |>> fun ((coms, sys), expr) ->
                            let systemType = match sys with
                                             | None -> Start
                                             | Some typ -> match typ with
                                                           | "Start" -> Start
                                                           | "Update" -> Update
                                                           | "Awake" -> Awake
                                                           | "End" -> End
                                                           | _ -> Exception "Invalid system run class" |> raise
                            SystemDeclaration((coms, systemType), expr)
    
    extensionRef.Value <- keyword "impl" >>. ws >>. identifier .>> ws .>> keyword ":=" .>> ws .>> pchar '{' .>> ws .>>. many1 (binding <|> typeDeclaration .>> ws) .>> ws .>> pchar '}' |>> Extension
    
    statement, binding, expression, typeDeclaration

let parseProgram = ws >>. sepBy1 statement ws