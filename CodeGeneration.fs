module CodeGeneration

open System.IO
open AST
open ParserLibrary.Core
open System.Collections.Generic
    
let standardLibrary = "
    enum Option<T> {
        Some(v:T);
        None;
    }

    typedef Vec3 = { x: Float, y: Float, z: Float}
    
    class Standard {
        public static function println(str: Any) { Sys.println(str); }
        public static function print(str: Any) { Sys.print(str); }
    }
    
    class StepIterator {
      var end:Int;
      var step:Int;
      var index:Int;

      public inline function new(start:Int, step:Int, end:Int) {
        this.index = start;
        this.end = end;
        this.step = step;
      }

      public inline function hasNext() return index < end;
      public inline function next() return (index += step) - step;
    }
"
    
let output = new StreamWriter("../../../ECS/Main.hx")
    
let activeComponents = List<string>()
let systems = List<string>()
    
let evalType (typ : Type option) = if typ.IsSome then
                                        match fst typ.Value with
                                        | Option o -> $": Option<{o}>"
                                        | _ -> $": {fst typ.Value}"
                                   else ""
    
let rec generateExpr expr =
    match expr with
    | BinaryLogicalExpr (e1, terms) ->
        generateExpr e1
            
        for op, e2 in terms do
            let operator = match op with
                           | And -> "&&"
                           | Or -> "||"
            output.Write($" {operator} ")
            generateExpr e2
    | BinaryComparisonExpr (((e1, op), e2), others) ->
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
        
        for e in others do
            match fst e with
            | ShortAnd -> output.Write(" && ")
            | ShortOr -> output.Write(" || ")
            
            generateExpr e1
            output.Write($" {operator} ")
            generateExpr (snd e)
            
    | BinaryArithmeticExpr (e1, terms) -> 
        if fst terms[0] <> Exp then
            generateExpr e1
            
            for op, e2 in terms do
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
            for op, e2 in terms do
                let operator = match op with
                               | Add -> "+"
                               | Sub -> "-"
                               | Mul -> "*"
                               | Div -> "/"
                               | Mod -> "%"
                               | _ -> ""
                output.Write($" {operator} ")
                generateExpr e2
            output.Write(")")
    | UnaryExpr (op, expr) ->
        match op with
        | Not -> output.Write("!")
        | Negative -> output.Write("-")
        generateExpr expr
    | IdentifierExpr iden ->
        if SemanticAnalysis.sets.Contains(iden) then
            output.Write($"{iden}.data")
        else output.Write(iden)
    | FunctionCallExpr (iden, exprs) ->
        if iden = "println" || iden = "print" then output.Write("Standard.")
        output.Write($"{iden}(")
        for e in exprs do
            match snd e with
            | LiteralExpr l ->
                match l with
                | VoidLiteral _ -> ()
                | _ ->
                    generateExpr (snd e)
                    if List.findIndex (fun expr -> e = expr) exprs <> exprs.Length - 1 then output.Write(",")
            | _ ->
                generateExpr (snd e)
                if List.findIndex (fun expr -> e = expr) exprs <> exprs.Length - 1 then output.Write(",")
        output.Write(")")
    | ArrayExpr (iden, expr) ->
        match expr with
        | RangeExpr ((e1, typ), e2) ->
            output.Write($"{iden}.slice(")
            generateExpr e1
            output.Write(", ")
            generateExpr e2
            if typ = Inclusive then output.Write(" + 1")
            output.Write(")")
        | _ ->
            output.Write($"{iden}[")
            generateExpr expr
            output.Write("]")
    | DataAccessExpr (iden1, iden2) ->
        if activeComponents.Contains(iden1) then
            output.Write($"cast(coms[{activeComponents.IndexOf(iden1)}], {iden1}).data.")
            generateExpr iden2
        else
            output.Write($"{iden1}.")
            generateExpr iden2
    | RangeExpr ((expr1, opType), expr2) ->
        match opType with
        | Inclusive | Exclusive ->
            generateExpr expr1
            output.Write("...")
            generateExpr expr2
            
            if opType = Inclusive then output.Write(" + 1")
        | ExclusiveStep expr15 | InclusiveStep expr15 ->
            output.Write($"new StepIterator(")
            generateExpr expr1
            output.Write(", ")
            generateExpr expr15
            output.Write(", ")
            generateExpr expr2
            match opType with | InclusiveStep _ -> output.Write(" + 1") | _ -> ()
            output.Write(")")
    | LiteralExpr lit ->
        match lit with
        | IntLiteral i -> output.Write(i)
        | FloatLiteral f -> output.Write(f)
        | DoubleLiteral d -> output.Write(d)
        | BoolLiteral b -> output.Write($"{b}".ToLower())
        | StringLiteral s -> output.Write($"'{s}'")
        | RuneLiteral c -> output.Write(c)
        | VoidLiteral _ -> output.Write("()")
        | CollectionLiteral colLit ->
            match colLit with
            | ArrayLiteral (colType, exprs) ->
                match colType with
                | Array ->
                    output.Write("[")
                    for expr in exprs do
                        generateExpr expr
                        if List.findIndex (fun e -> e = expr) exprs <> exprs.Length - 1 then output.Write(", ")
                    output.Write("]")
                | Set ->
                    output.Write("new Set([")
                    for expr in exprs do
                        generateExpr expr
                        if List.findIndex (fun e -> e = expr) exprs <> exprs.Length - 1 then output.Write(", ")
                    output.Write("])")
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
    | ComponentAccessExpr (i1, i2) ->
        if i1 = "break" then
            output.WriteLine($"{i2} = true")
    | Parenthesis expr ->
        output.Write("(")
        generateExpr expr
        output.Write(")")
    | Lambda (idens, stats) ->
        output.Write("function(")
        for i in idens do
            output.Write(i)
            if List.findIndex (fun va -> va = i) idens <> idens.Length - 1 then output.Write(",")
        output.Write(") { ")
        for stat in stats do
            if List.findIndex (fun e -> e = stat) stats = stats.Length - 1 then output.Write("return ")
            generateStatement stat
        output.Write(" }")

and generateStatement stat =
    match stat with
    | Binding bind ->
        match bind with
        | ImmutableBinding ((iden, typ), expr) ->
            let exType = evalType typ
            output.Write($"var {iden} {exType} = ")
            generateExpr expr
            output.WriteLine(";")
        | MutableBinding ((iden, typ), expr) ->
            let exType = evalType typ
            output.Write($"var {iden} {exType} = ")
            generateExpr expr
            output.WriteLine(";")
        | FunctionDeclaration _ -> ()
        | Reassignment ((iden, op), expr) ->
            generateExpr iden
            output.Write($" = ")
            
            if op.IsSome && op.Value = Exp then
                output.Write($"Math.pow(")
                generateExpr iden
                output.Write(", ")
                generateExpr expr
                output.Write(");")
            else
                if op.IsSome then
                    generateExpr iden
                    output.Write(match op.Value with
                                 | Add -> "+"
                                 | Sub -> "-"
                                 | Mul -> "*"
                                 | Div -> "/"
                                 | Mod -> "%"
                                 | _ -> "")
                
                generateExpr expr
                output.WriteLine(";")
        | EntityBinding (iden, coms) ->
            output.Write("EntityManager.addEntity([")
            for c in coms do
                output.Write($"new {fst c}(")
                if (snd c).IsSome then generateExpr (snd c).Value
                output.Write(")")
                if List.findIndex (fun e -> e = c) coms <> coms.Length - 1 then output.Write(",")
            output.WriteLine("]);")
    | Statement.Expression expr ->
        match expr with
        | IfExpr ((i, ei), e) ->
            match fst (fst i) with
            | Expr e ->
               output.Write("if (")
               generateExpr e
               output.WriteLine(") {")
            | LetStatement iden ->
               output.WriteLine($"var val = switch ({iden}) " + "{")
               output.WriteLine("\tcase None: false;")
               output.WriteLine("\tcase Some(v): true; }")
               output.Write("if (val) {")
                           
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
        | ForExpr ((((label, iden), expr), where), stats) ->
            if label.IsSome then output.WriteLine($"var {label.Value} = false;")
            
            match iden with
            | Identifier i -> 
                output.Write($"for ({i} in ")
                generateExpr expr
                output.WriteLine(") {")
            | MapDestructuring (key, value) ->
                output.Write($"for ({key} in ")
                generateExpr expr
                output.WriteLine(".keys()) {")
                output.Write($"\tvar {value} = ")
                generateExpr expr
                output.WriteLine($"[{key}];")
                
            if where.IsSome then
                output.Write("\tif (")
                generateExpr where.Value
                output.WriteLine(") {")
                
            for s in stats do
                generateStatement s
            
            if label.IsSome then output.WriteLine($"if ({label.Value} == true) " + "{ break; }")
            
            if where.IsSome then output.Write("}")
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
                | Option o -> output.Write($"Option<{o}>")
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
                        | Option o -> output.Write($"Option<{o}>")
                        | s -> output.Write(s)
                        if List.findIndex (fun ty -> ty = t) types <> types.Length - 1 then output.Write(", ")
                    output.WriteLine(");")
            output.WriteLine("}\n")
        | ComponentDeclaration (iden, data) ->
            output.WriteLine($"class {iden} extends Component " + "{")
            
            let dataType =
                data
                |> List.map (fun (iden, typ) ->
                             let t = match fst typ with
                                     | Custom s -> s
                                     | Option o -> $"Option<{o}>"
                                     | s -> $"{s}"
                             $"{iden}: {t}, ")
                |> List.reduce (+)
                |> string
            
            output.WriteLine("\tpublic var data : {" + dataType[..dataType.Length - 2] + "};\n")
            output.WriteLine("public function new(d) { data = d; " + $"this.name = '{iden}'; " + "}")
            output.WriteLine("}\n")
        | SystemDeclaration ((coms, typ), stats) ->
            for c in coms do
                activeComponents.Add(c)
            
            let rnd = System.Random()
            let name = $"{rnd.Next(65, 90) |> char}{rnd.Next(97, 122)}{rnd.Next(97, 122)}"
            systems.Add(name)
            
            output.WriteLine($"class {name} extends System " + "{")
            output.WriteLine("public function new() " + "{")
            output.WriteLine($"\tthis.type = {typ};" + "\n}\n")
            output.WriteLine("public function run() : Void {")
            output.WriteLine("\tfor (entity in EntityManager.entities) {")
            
            output.Write("\t\tvar coms = hasAllComponents(entity, [")
            for c in coms do output.Write($"'{c}', ")
            output.WriteLine("]);")
            
            output.WriteLine("\t\tif (coms.length == entity.length) {")
            
            for stat in stats do
                generateStatement stat
                
            output.WriteLine("}}}}")
            
            for c in coms do activeComponents.Remove(c) |> ignore
        | TypeAlias tuple -> failwith "todo"
        | Extension (iden, stats) ->
            output.WriteLine($"class {iden}Extender " + "{")
            for stat in stats do
                generateStatement stat
            output.WriteLine("}")
    | _ -> ()

let generateFunction iden params retTyp exprs =
    let param = seq { for p in params ->
                        match p with
                        | Unspecified iden -> $"{iden}, "
                        | Specified ((_, iden), typ) ->
                            if typ.IsSome then
                                match fst typ.Value with
                                | Option o -> $"{iden}: Option<{o}>,"
                                | _ -> $"{iden}: {fst typ.Value},"
                            else $"{iden}, "
                    } |> List.ofSeq |> string |> String.filter (fun c -> c <> ';' && c <> '[') 
    output.WriteLine($"public static function {iden}({param[..param.Length - 4]})" + "{") //  : {fst (snd SemanticAnalysis.functions[iden])}
    for expr in exprs do
        if List.findIndex (fun e -> e = expr) exprs = exprs.Length - 1 then output.Write("return ")
        generateStatement expr
    output.WriteLine("}")

let generate code =
    match code with
    | Success (ast, _) ->
        output.WriteLine("import Entity; import System; import Set;")
        
        for stat in ast do
            match stat with
            | TypeDeclaration decl ->
                match decl with
                | Extension (iden, _) -> output.WriteLine($"using Main.{iden}Extender;\n")
                | _ -> ()
            | _ -> ()
        
        output.WriteLine(standardLibrary)
        
        for statement in ast do
            generateType statement
        
        output.WriteLine("class Main {")
        
        for stat in ast do
            match stat with
            | Binding bind ->
                match bind with
                | FunctionDeclaration (((iden, params), retType), exprs) -> generateFunction iden params retType exprs
                | _ -> ()
            | _ -> ()
        
        output.WriteLine("\tpublic static function main() {")
        
        for statement in ast do
            generateStatement statement
                
        output.WriteLine("\t\tvar systems = new SystemManager();")
        for system in systems do
            output.WriteLine($"\t\tsystems.addSystem(new {system}());")
        output.WriteLine("\t\tsystems.run();")
                
        output.WriteLine("}}")
    | Failure _ -> printfn "Errors with parsing. Could not compile code"
    
    output.Close()