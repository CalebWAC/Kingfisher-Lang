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

let validateBindings exprs =      
    for expr in exprs do
       match expr with
       | Expression e ->
            traverse e
       | _ ->()

let analyzeBindings (ast : ParseResult<'a>) =
    match ast with
    | Success (a, _) ->
        for statement in a do
            match statement with
            | Binding bind ->
                match bind with
                | ImmutableBinding immut ->
                    variables.Add(fst (fst immut), snd (fst immut))
                    snd immut |> validateBindings
                | MutableBinding mut -> variables.Add(fst (fst mut), snd (fst mut))
                | FunctionDeclaration func ->
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
                | Reassignment res -> ()
                | EntityBinding ent -> ()
                | SystemDeclaration sys -> ()
            | Statement.Expression expr ->
                match expr with
                | ForExpr (((_, _), _), exprs) -> for e in exprs do traverse e
                | _ -> ()
            | TypeDeclaration decl -> ()
    | Failure _ -> ()
    
    for i in variables do printf $"{i}\t"
    printfn ""
    for i in functions do printf $"{i}\t"