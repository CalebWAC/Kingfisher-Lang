module CodeGeneration

open System.IO
open AST
open ParserLibrary.Core
    
let standardLibrary = "
    static function println(str) { Sys.println(str); }
"
    
let output = new StreamWriter("Main.hx")
    
let rec generateExpr expr =
    match expr with
    | BinaryLogicalExpr ((e1, op), e2) ->
        generateExpr e1
        let operator = match op with
                       | And -> "&&"
                       | Or -> "||"
        output.Write($" {operator} ")
        generateExpr e2
    | BinaryComparisonExpr ((e1, op), e2) ->
        generateExpr e1
        let operator = match op with
                       | Equal -> "=="
                       | NotEqual -> "!="
                       | LessThan -> "<"
                       | GreaterThan -> ">"
                       | LessEqual -> "<="
                       | GreaterEqual -> ">="
        output.Write($" {operator} ")
        generateExpr e2
    | BinaryArithmeticExpr ((e1, op), e2) ->
        if op <> Exp then
            generateExpr e1
            let operator = match op with
                           | Add -> "+"
                           | Sub -> "-"
                           | Mul -> "*"
                           | Div -> "/"
                           | Mod -> "%"
                           | _ -> ""
            output.Write($" {operator} ")
            generateExpr e2
        else
            output.Write("Math.pow(")
            generateExpr e1
            output.Write(", ")
            generateExpr e2
            output.Write(")")
    | UnaryExpr (op, expr) ->
        match op with
        | Not -> output.Write("!")
        | Negative -> output.Write("-")
        generateExpr expr
    | IdentifierExpr iden -> output.Write(iden)
    | FunctionCallExpr (iden, exprs) ->
        output.Write($"{iden}(")
        for e in exprs do
            generateExpr e
            if List.findIndex (fun expr -> e = expr) exprs <> exprs.Length - 1 then output.Write(",")
        output.Write(")")
    | ArrayExpr (iden, expr) ->
        output.Write($"{iden}[")
        generateExpr expr
        output.Write("]")
    | DataAccessExpr (iden1, iden2) -> output.Write($"{iden1}.{iden2}")
    | LiteralExpr lit ->
        match lit with
        | IntLiteral i -> output.Write(i)
        | FloatLiteral f -> output.Write(f)
        | BoolLiteral b -> output.Write($"{b}".ToLower())
        | StringLiteral s -> output.Write(s)
        | RuneLiteral c -> output.Write(c)
        | VoidLiteral -> ()
        | CollectionLiteral colLit ->
            match colLit with
            | ArrayLiteral (_, exprs) ->
                output.Write("[")
                for expr in exprs do
                    generateExpr expr
                    if List.findIndex (fun e -> e = expr) exprs <> exprs.Length - 1 then output.Write(",")
                output.Write("]")
            | MapLiteral exprs ->
                output.Write("[")
                for key, value in exprs do
                    generateExpr key
                    output.Write(" => ")
                    generateExpr value
                    if List.findIndex (fun e -> (key, value) = e) exprs <> exprs.Length - 1 then output.Write(",")
                output.Write("]")
        | RecordLiteral(_, values) ->
            output.Write("{")
            for v in values do
                output.Write($"{fst v}: ")
                generateExpr (snd v)
                if List.findIndex (fun va -> va = v) values <> values.Length - 1 then output.Write(",")
            output.Write("}")
        | TupleLiteral expressions -> ()
    | _ -> ()

let generate code =
    match code with
    | Success (ast, _) ->
        output.WriteLine("class Main {")
        output.WriteLine(standardLibrary)
        output.WriteLine("\tpublic static function main() {")
        
        for statement in ast do
            match statement with
            | Binding bind ->
                match bind with
                | ImmutableBinding ((iden, typ), expr) ->
                    let exType = if typ.IsSome then $": {fst typ.Value}" else ""
                    output.Write($"var {iden} {exType} = ")
                    generateExpr expr
                    output.WriteLine(";")
                | MutableBinding ((iden, typ), expr) ->
                    let exType = if typ.IsSome then $": {fst typ.Value}" else ""
                    output.Write($"var {iden} {exType} = ")
                    generateExpr expr
                    output.WriteLine(";")
                | FunctionDeclaration (((iden, params), retType), exprs) ->
                    let param = seq { for p in params ->
                                        match p with
                                        | Unspecified iden -> $"{iden}, "
                                        | Specified ((_, iden), typ) -> $"{iden}: {fst typ.Value},"
                                    } |> List.ofSeq |> string |> String.filter (fun c -> c <> ';' && c <> '[') 
                    output.WriteLine($"function {iden}({param[..param.Length - 3]}) : {fst (snd SemanticAnalysis.functions[iden])}" + "{")
                    for expr in exprs do
                        if List.findIndex (fun e -> e = expr) exprs = exprs.Length - 1 then output.Write("return ")
                        generateExpr expr
                        output.WriteLine(";")
                    output.WriteLine("}")
                | Reassignment tuple -> failwith "todo"
                | EntityBinding _ -> ()
                | SystemDeclaration _ -> ()
            | Statement.Expression expr -> ()
            | TypeDeclaration _ -> ()
                
        output.WriteLine("}}")
    | Failure _ -> printfn "Errors with parsing. Could not compile code"
    
    output.Close()