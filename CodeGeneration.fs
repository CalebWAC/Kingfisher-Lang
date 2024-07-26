module CodeGeneration

open System.IO
open AST
open ParserLibrary.Core
    
use output = new StreamWriter("Program.hx")
    
let rec generateExpr expr =
    match expr with
    | BinaryLogicalExpr ((e1, op), e2) -> ()
    | BinaryComparisonExpr ((e1, op), e2) -> ()
    | BinaryArithmeticExpr ((e1, op), e2) -> ()
    | UnaryExpr (op, expr) -> ()
    | IdentifierExpr iden -> ()
    | FunctionCallExpr (iden, exprs) -> ()
    | ArrayExpr (iden, expr) -> ()
    | DataAccessExpr (iden1, iden2) -> ()
    | LiteralExpr lit ->
        match lit with
        | IntLiteral i -> output.Write(i)
        | FloatLiteral f -> output.Write(f)
        | BoolLiteral b -> output.Write(b)
        | StringLiteral s -> output.Write(s)
        | RuneLiteral c -> output.Write(c)
        | VoidLiteral -> ()
        | CollectionLiteral colLit ->
            match colLit with
            | ArrayLiteral (_, exprs) -> ()
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
        printfn "About to print"
        output.WriteLine("class Main {")
        
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
                                    } |> List.ofSeq |> string
                    output.WriteLine($"static public function {iden}({param[..param.Length - 2]}) " + "{")
                    for expr in exprs do
                        generateExpr expr
                        output.WriteLine(";")
                    output.WriteLine("}")
                | Reassignment tuple -> failwith "todo"
                | EntityBinding _ -> ()
                | SystemDeclaration _ -> ()
            | Statement.Expression expr -> ()
            | TypeDeclaration _ -> ()
                
        output.WriteLine("}")
    | Failure _ -> printfn "Errors with parsing. Could not compile code"
    
    output.Close()