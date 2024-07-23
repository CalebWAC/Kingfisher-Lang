module SemanticAnalysis

open System.Collections.Generic
open AST
open ParserLibrary.Core

let variables = Dictionary<string, TypeKeyWord>()
let functions = Dictionary<string, TypeKeyWord list * TypeKeyWord>()

let analyzeBindings (ast : ParseResult<'a>) =
    match ast with
    | Success (a, _) ->
        for statement in a do
            printfn $"{statement.GetType()}"
    | Failure _ -> ()