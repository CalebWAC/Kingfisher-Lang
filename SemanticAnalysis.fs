module SemanticAnalysis

open AST
open ParserLibrary.Core
open System.Collections.Generic

let constants = Dictionary<string, Type option>()
let variables = Dictionary<string, Type option>()

let unionValues = Dictionary<string, Type>()
let customTypes = List<string>()

let functions = Dictionary<string, Type option list * Type>()
functions.Add ("println", ([Some(String, None)], (Void, None)))

let checkType typ =
    match fst typ with
    | Custom s ->
        if customTypes.Contains(s) |> not then
            printfn $"Type {s} does not exist"
    | _ -> ()

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
        if variables.ContainsKey(iden) |> not && constants.ContainsKey(iden) |> not && unionValues.ContainsKey(iden) |> not then
            printfn $"{iden} does not exist"; (false, Type(Void, None))
        else (true, if variables.ContainsKey(iden) then variables[iden].Value
                    elif constants.ContainsKey(iden) then constants[iden].Value
                    else unionValues[iden])
    | FunctionCallExpr (iden, exprs) ->
        if functions.ContainsKey(iden) |> not && unionValues.ContainsKey(iden) |> not then
            printfn $"{iden} does not exist"; (false, Type(Void, None))
        else
            if functions.ContainsKey(iden) then
                let callValid = exprs 
                                |> List.forall (fun expr ->
                                    snd (traverse expr) = ((fst(functions[iden]))[exprs |> List.findIndex (fun e -> e = expr)]).Value)
                (callValid, (snd functions[iden]))
            else (true, unionValues[iden])
    | ArrayExpr (iden, _) ->
        if variables.ContainsKey(iden) |> not && constants.ContainsKey(iden) |> not then
            printfn $"{iden} does not exist"; (false, Type(Void, None))
        else (true, if variables.ContainsKey(iden) then (fst variables[iden].Value, None) else (fst constants[iden].Value, None))
    | DataAccessExpr (iden, _) ->
        if variables.ContainsKey(iden) |> not && constants.ContainsKey(iden) |> not then
            printfn $"{iden} does not exist"; (false, Type(Void, None))
        else (true, if variables.ContainsKey(iden) then variables[iden].Value else constants[iden].Value)
    | ComponentAccessExpr (iden, _) ->
        if variables.ContainsKey(iden) |> not && constants.ContainsKey(iden) |> not then
            printfn $"{iden} does not exist"; (false, Type(Void, None))
        else (true, if variables.ContainsKey(iden) then variables[iden].Value else constants[iden].Value)
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
    
let rec validateStatement statement =
    match statement with
    | Binding bind ->
        match bind with
        | ImmutableBinding immut ->
            let valid, typ = snd immut |> traverse
            checkType typ
            if valid then constants.Add(fst (fst immut), Some typ)
        | MutableBinding mut ->
            let valid, typ = snd mut |> traverse
            checkType typ
            if valid then variables.Add(fst (fst mut), Some typ)
        | FunctionDeclaration func ->
            let param = [ for x in snd (fst (fst func)) do
                                 match x with
                                 | Unspecified iden ->
                                     constants.Add(iden, None)
                                     yield None
                                 | Specified ((_, iden), typOpt) ->
                                     checkType typOpt.Value
                                     constants.Add(iden, typOpt)
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
                    constants.Remove(iden) |> ignore
                | Specified ((_, iden), _) ->
                    constants.Remove(iden) |> ignore
        | Reassignment res ->
            if variables.ContainsKey(fst res) |> not then
                if constants.ContainsKey(fst res) then
                    printfn $"{fst res} is not mutable"
                else printfn $"{fst res} does not exist"
            snd res |> traverse |> ignore
        | EntityBinding ent -> ()
        | SystemDeclaration sys -> ()
    | Statement.Expression expr ->
        match expr with
        | ForExpr ((((_, iden), e), _), exprs) ->
            constants.Add(iden, snd(traverse e) |> Some)
            for e in exprs do validateStatement e
            constants.Remove(iden) |> ignore
        | WhileExpr (expr, exprs) ->
            traverse expr |> ignore
            for e in exprs do validateStatement e
        | IfExpr ((e, el), els) ->
            let evalIf ifExpr =
                match fst (fst ifExpr) with
                | Expr ex -> traverse ex |> ignore
                | LetStatement (iden, expr) ->
                    traverse expr |> ignore
                    constants.Add(iden, None)
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
                for statement in snd case do
                    validateStatement statement
        | Expression e -> traverse e |> ignore
    | TypeDeclaration decl ->
        match decl with
        | RecordDeclaration (iden, _) ->
            customTypes.Add(iden)
        | UnionDeclaration (iden, cases) ->
            customTypes.Add(iden)
            for c in cases do
                match c with
                | Single s -> unionValues.Add(s, (Custom iden, None))
                | Multiple (s, _) -> unionValues.Add(s, (Custom iden, None))
        | ComponentDeclaration tuple -> failwith "todo"
        | TypeAlias tuple -> failwith "todo"
        | Extension tuple -> failwith "todo"

let analyze ast =
    match ast with
    | Success (a, _) ->
        for statement in a do
            validateStatement statement
    | Failure _ -> ()
    
    //for i in variables do printf $"{i}\t"
    //printfn ""
    //for i in functions do printf $"{i}\t"