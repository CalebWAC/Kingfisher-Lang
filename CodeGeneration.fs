module CodeGeneration

open System.IO
open AST
open ParserLibrary.Core
    
let standardLibrary = "
    static function println(str: Any) { Sys.println(str); }
    static function print(str: Any) { Sys.print(str); }
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
    | RangeExpr ((expr1, opType), expr2) ->
        generateExpr expr1
        output.Write("...")
        generateExpr expr2
        
        if opType = Inclusive then output.Write(" + 1")
    | LiteralExpr lit ->
        match lit with
        | IntLiteral i -> output.Write(i)
        | FloatLiteral f -> output.Write(f)
        | BoolLiteral b -> output.Write($"{b}".ToLower())
        | StringLiteral s -> output.Write($"\"{s}\"")
        | RuneLiteral c -> output.Write(c)
        | VoidLiteral -> ()
        | CollectionLiteral colLit ->
            match colLit with
            | ArrayLiteral (_, exprs) ->
                output.Write("[")
                for expr in exprs do
                    generateExpr expr
                    if List.findIndex (fun e -> e = expr) exprs <> exprs.Length - 1 then output.Write(", ")
                output.Write("]")
            | MapLiteral exprs ->
                output.Write("[")
                for key, value in exprs do
                    generateExpr key
                    output.Write(" => ")
                    generateExpr value
                    if List.findIndex (fun e -> (key, value) = e) exprs <> exprs.Length - 1 then output.Write(", ")
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

let rec generateStatement stat =
    match stat with
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
            output.WriteLine($"function {iden}({param[..param.Length - 4]})" + "{") //  : {fst (snd SemanticAnalysis.functions[iden])}
            for expr in exprs do
                if List.findIndex (fun e -> e = expr) exprs = exprs.Length - 1 then output.Write("return ")
                generateStatement expr
            output.WriteLine("}")
        | Reassignment ((iden, op), expr) ->
            let iden = match iden with
                       | IdentifierExpr s -> s
                       | DataAccessExpr(s, s1) -> $"{s}.{s1}"
                       | _ -> ""
            
            output.Write($"{iden} = ")
            
            
            if op.IsSome && op.Value = Exp then
                output.Write($"Math.pow({iden}, ")
                generateExpr expr
                output.Write(");")
            else
                if op.IsSome then
                    output.Write($"{iden} ")
                    output.Write(match op.Value with
                                 | Add -> "+"
                                 | Sub -> "-"
                                 | Mul -> "*"
                                 | Div -> "/"
                                 | Mod -> "%"
                                 | _ -> "")
                
                generateExpr expr
                output.WriteLine(";")
        | EntityBinding _ -> ()
    | Statement.Expression expr ->
        match expr with
        | IfExpr ((i, ei), e) ->
            output.Write("if (")
            let expr = match fst (fst i) with
                       | Expr e -> generateExpr e
                       | _ -> ()
            output.WriteLine($"{expr}) " + "{")
            for e in snd i do
                generateStatement e
            output.WriteLine("}")
            
            if ei.IsSome then
                for els in ei.Value do
                    output.Write("else if (")
                    let expr = match fst (fst els) with
                               | Expr e -> generateExpr e
                               | _ -> ()
                    output.WriteLine($"{expr}) " + "{")
                    for e in snd els do
                        generateStatement e
                    output.WriteLine("}")
                    
            if e.IsSome then
                output.Write("else {")
                for ex in e.Value do
                    generateStatement ex
                output.WriteLine("}")
        | ForExpr ((((_, iden), expr), _), stats) ->
            output.Write($"for ({iden} in ")
            generateExpr expr
            output.WriteLine(") {")
            for s in stats do
                generateStatement s
            output.WriteLine("}")
        | WhileExpr (expr, exprs) ->
            output.Write("while (")
            generateExpr expr
            output.Write(") {")
            for e in exprs do generateStatement e 
            output.WriteLine("}")
        | MatchExpr (iden, cases) ->
            output.Write($"switch ({iden}) " + "{")
            for case in cases do
                output.Write("case ")
                generateExpr (fst case)
                output.Write(": ")
                for c in snd case do
                    generateStatement c
            output.WriteLine("}")
        | Expression expression ->
            generateExpr expression
            output.WriteLine(";")
    | _ -> ()

let generateType statement =
    match statement with
    | TypeDeclaration decl ->
        match decl with
        | RecordDeclaration (iden, values) ->
            output.Write($"typedef {iden} = " + "{")
            for v in values do
                output.Write($"var {snd(fst v)}: ")
                match fst (snd v) with
                | Custom s -> output.Write(s)
                | s -> output.Write(s)
                output.Write("; ")
            output.WriteLine("}")
        | UnionDeclaration (iden, cases) ->
            output.WriteLine($"enum {iden} " + "{")
            
            for case in cases do
                match case with
                | Single iden -> output.WriteLine($"{iden};")
                | Multiple(iden, types) ->
                    output.Write($"{iden}(")
                    for t in types do
                        let rnd = System.Random()
                        let name = $"{rnd.Next(97, 122) |> char}{rnd.Next(97, 122)}{rnd.Next(97, 122)}"
                        
                        output.Write($"{name}: ")
                        match fst t with
                        | Custom s -> output.Write(s)
                        | s -> output.Write(s)
                        if List.findIndex (fun ty -> ty = t) types <> types.Length - 1 then output.Write(", ")
                    output.WriteLine(");")
            output.WriteLine("}\n")
        | ComponentDeclaration tuple -> failwith "todo"
        | SystemDeclaration _ -> ()
        | TypeAlias tuple -> failwith "todo"
        | Extension tuple -> failwith "todo"
    | _ -> ()

let generate code =
    match code with
    | Success (ast, _) ->
        for statement in ast do
            generateType statement
        
        output.WriteLine("class Main {")
        output.WriteLine(standardLibrary)
        output.WriteLine("\tpublic static function main() {")
        
        for statement in ast do
            generateStatement statement
                
        output.WriteLine("}}")
    | Failure _ -> printfn "Errors with parsing. Could not compile code"
    
    output.Close()