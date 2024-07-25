module SemanticAnalysis

open AST
open ParserLibrary.Core
open System.Collections.Generic

let variables = Dictionary<string, Type option>()
let functions = Dictionary<string, Type option list * Type option>()

let rec traverse expr =
        match expr with
        | BinaryLogicalExpr (e1, e2) -> traverse (fst e1); traverse e2
        | BinaryComparisonExpr (e1, e2) -> traverse (fst e1); traverse e2
        | BinaryArithmeticExpr (e1, e2) -> traverse (fst e1); traverse e2
        | UnaryExpr (_, e) -> traverse e
        | IdentifierExpr iden -> if variables.ContainsKey(iden) |> not then printfn $"{iden} does not exist"
        | FunctionCallExpr (iden, _) -> if functions.ContainsKey(iden) |> not then printfn $"{iden} does not exist"
        | ArrayExpr (iden, _) -> if variables.ContainsKey(iden) |> not then printfn $"{iden} does not exist"
        | DataAccessExpr (iden, _) -> if variables.ContainsKey(iden) |> not then printfn $"{iden} does not exist"
        | ComponentAccessExpr (iden, _) -> if variables.ContainsKey(iden) |> not then printfn $"{iden} does not exist"
        | _ -> ()

let validateBindings expr =      
    match expr with
    | Expression e ->
        traverse e
    | _ ->()
    
let rec validateStatement statement =
    match statement with
    | Binding bind ->
        match bind with
        | ImmutableBinding immut ->
            snd immut |> traverse
            variables.Add(fst (fst immut), snd (fst immut))
        | MutableBinding mut ->
            snd mut |> traverse
            variables.Add(fst (fst mut), snd (fst mut))
        | FunctionDeclaration func ->
            for e in snd func do traverse e
            let param = seq { for x in snd (fst (fst func)) do
                                 match x with
                                 | Unspecified iden ->
                                     variables.Add(iden, None)
                                     yield None
                                 | Specified ((_, iden), typOpt) ->
                                     variables.Add(iden, typOpt)
                                     yield typOpt
                        }
            functions.Add(fst (fst (fst func)), (Seq.toList param, (snd (fst func))))
        | Reassignment res ->
            if variables.ContainsKey(fst res) |> not then printfn $"{fst res} does not exist"
            snd res |> traverse
        | EntityBinding ent -> ()
        | SystemDeclaration sys -> ()
    | Statement.Expression expr ->
        match expr with
        | ForExpr ((((_, iden), _), _), exprs) ->
            variables.Add(iden, None)
            for e in exprs do validateStatement e
            variables.Remove(iden) |> ignore
        | WhileExpr (expr, exprs) ->
            traverse expr
            for e in exprs do validateBindings e
        | IfExpr ((e, el), els) ->
            let evalIf ifExpr =
                match fst (fst ifExpr) with
                | Expr ex -> traverse ex
                | LetStatement (iden, expr) ->
                    traverse expr
                    variables.Add(iden, None)
                for expr in snd ifExpr do traverse expr
            
            evalIf e
            match el with
            | Some el -> for elifExpr in el do evalIf elifExpr
            | None -> ()
            
            match els with
            | Some els -> for elsIf in els do traverse elsIf
            | None -> ()
        | MatchExpr (_, cases) ->
            for case in cases do
                snd case |> validateBindings
        | Expression e -> traverse e
    | TypeDeclaration decl -> ()

let analyzeBindings (ast : ParseResult<'a>) =
    match ast with
    | Success (a, _) ->
        for statement in a do
            validateStatement statement
    | Failure _ -> ()
    
    for i in variables do printf $"{i}\t"
    printfn ""
    for i in functions do printf $"{i}\t"