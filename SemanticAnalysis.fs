module SemanticAnalysis

open AST
open ParserLibrary.Core
open System.Collections.Generic

let variables = Dictionary<string, Type option>()

let functions = Dictionary<string, Type option list * Type>()
functions.Add ("println", ([Some(String, None)], (Void, None)))

let rec traverse expr =
        match expr with
        | BinaryLogicalExpr (e1, e2) ->
            let t1 = traverse (fst e1)
            let t2 = traverse e2
            if fst t1 && fst t2 then
                (snd t1 = snd t2, snd t1)
            else (false, Type(Void, None))
        | BinaryComparisonExpr (e1, e2) ->
            let t1 = traverse (fst e1)
            let t2 = traverse e2
            if fst t1 && fst t2 && snd t1 = snd t2 then
                (true, Type(Bool, None))
            else (false, Type(Void, None))
        | BinaryArithmeticExpr (e1, e2) ->
            let t1 = traverse (fst e1)
            let t2 = traverse e2
            if fst t1 && fst t2 then
                (snd t1 = snd t2, snd t1)
            else (false, Type(Void, None))
        | UnaryExpr (_, e) -> traverse e
        | IdentifierExpr iden ->
            if variables.ContainsKey(iden) |> not then
                printfn $"{iden} does not exist"; (false, Type(Void, None))
            else (true, variables[iden].Value)
        | FunctionCallExpr (iden, exprs) ->
            if functions.ContainsKey(iden) |> not then
                printfn $"{iden} does not exist"; (false, Type(Void, None))
            else
                let callValid = exprs 
                                |> List.forall (fun expr ->
                                    snd (traverse expr) = ((fst(functions[iden]))[exprs |> List.findIndex (fun e -> e = expr)]).Value)
                (callValid, (snd functions[iden]))
        | ArrayExpr (iden, _) ->
            if variables.ContainsKey(iden) |> not then
                printfn $"{iden} does not exist"; (false, Type(Void, None))
            else (true, (fst variables[iden].Value, None))
        | DataAccessExpr (iden, _) ->
            if variables.ContainsKey(iden) |> not then
                printfn $"{iden} does not exist"; (false, Type(Void, None))
            else (true, variables[iden].Value)
        | ComponentAccessExpr (iden, _) ->
            if variables.ContainsKey(iden) |> not then
                printfn $"{iden} does not exist"; (false, Type(Void, None))
            else (true, variables[iden].Value)
        | LiteralExpr lit ->
            let litToType = match lit with
                            | IntLiteral _ -> Type (Int, None)
                            | FloatLiteral _ -> Type (Float, None)
                            | BoolLiteral _ -> Type (Bool, None)
                            | StringLiteral _ -> Type (String, None)
                            | RuneLiteral _ -> Type (Rune, None)
                            | CollectionLiteral c -> match c with
                                                     | ArrayLiteral (col, _) -> Type(Int, Some col)
                                                     | MapLiteral _ -> Type(Map, None)
                            | _ -> Type (Void, None)
            (true, litToType)
        | _ -> (true, Type(Void, None))

let validateBindings expr =      
    match expr with
    | Expression e ->
        traverse e
    | _ -> (false, Type(Void, None))
    
let rec validateStatement statement =
    match statement with
    | Binding bind ->
        match bind with
        | ImmutableBinding immut ->
            let valid, typ = snd immut |> traverse
            if valid then variables.Add(fst (fst immut), Some typ)
        | MutableBinding mut ->
            let valid, typ = snd mut |> traverse
            if valid then variables.Add(fst (fst mut), Some typ)
        | FunctionDeclaration func ->
            let param = [ for x in snd (fst (fst func)) do
                                 match x with
                                 | Unspecified iden ->
                                     variables.Add(iden, None)
                                     yield None
                                 | Specified ((_, iden), typOpt) ->
                                     variables.Add(iden, typOpt)
                                     yield typOpt
                        ]
            for e in snd func do traverse e |> ignore
            let retType = match snd (fst func) with
                          | Some ret -> ret
                          | None -> (snd func)[(snd func).Length - 1] |> traverse |> snd
            
            functions.Add(fst (fst (fst func)), (Seq.toList param, retType))
            for x in snd(fst (fst func)) do
                match x with
                | Unspecified iden ->
                    variables.Remove(iden) |> ignore
                | Specified ((_, iden), _) ->
                    variables.Remove(iden) |> ignore
        | Reassignment res ->
            if variables.ContainsKey(fst res) |> not then printfn $"{fst res} does not exist"
            snd res |> traverse |> ignore
        | EntityBinding ent -> ()
        | SystemDeclaration sys -> ()
    | Statement.Expression expr ->
        match expr with
        | ForExpr ((((_, iden), e), _), exprs) ->
            variables.Add(iden, snd(traverse e) |> Some)
            for e in exprs do validateStatement e
            variables.Remove(iden) |> ignore
        | WhileExpr (expr, exprs) ->
            traverse expr |> ignore
            for e in exprs do validateStatement e
        | IfExpr ((e, el), els) ->
            let evalIf ifExpr =
                match fst (fst ifExpr) with
                | Expr ex -> traverse ex |> ignore
                | LetStatement (iden, expr) ->
                    traverse expr |> ignore
                    variables.Add(iden, None)
                for expr in snd ifExpr do validateStatement expr
            
            evalIf e
            match el with
            | Some el -> for elifExpr in el do evalIf elifExpr
            | None -> ()
            
            match els with
            | Some els -> for elsIf in els do validateStatement elsIf
            | None -> ()
        | MatchExpr (_, cases) ->
            for case in cases do
                snd case |> validateStatement
        | Expression e -> traverse e |> ignore
    | TypeDeclaration decl -> ()

let analyze ast =
    match ast with
    | Success (a, _) ->
        for statement in a do
            validateStatement statement
    | Failure _ -> ()
    
    //for i in variables do printf $"{i}\t"
    //printfn ""
    //for i in functions do printf $"{i}\t"