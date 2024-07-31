module SemanticAnalysis

open AST
open ParserLibrary.Core
open System.Collections.Generic

let constants = Dictionary<string, Type option>()
let variables = Dictionary<string, Type option>()

let unionValues = Dictionary<string, Type>()
unionValues.Add("Some", Type(Option Void, None))
unionValues.Add("None", Type(Option Void, None))

let customTypes = Dictionary<string, (string * Type) list option>()
customTypes.Add("Vec3", Some ["x", Type(Float, None); "y", Type(Float, None); "z", Type(Float, None)])

let components = Dictionary<string,  (string * Type) list option>()
let activeComponents = List<string>()

let functions = Dictionary<string, Type option list * Type>()
functions.Add ("println", ([Some(String, None)], (Void, None)))
functions.Add ("print", ([Some(String, None)], (Void, None)))

let checkType typ =
    match fst typ with
    | Custom s ->
        if customTypes.ContainsKey(s) |> not then
            printfn $"Type {s} does not exist"
    | _ -> ()

let rec validateExpr expr =
    match expr with
    | BinaryLogicalExpr (e1, e2) ->
        let t1 = validateExpr (fst e1)
        let t2 = validateExpr e2
        if fst t1 && fst t2 then
            (snd t1 = snd t2, snd t1)
        else (false, Type(Void, None))
    | BinaryComparisonExpr (e1, e2) ->
        let t1 = validateExpr (fst e1)
        let t2 = validateExpr e2
        if fst t1 && fst t2 && snd t1 = snd t2 then
            (true, Type(Bool, None))
        else (false, Type(Void, None))
    | BinaryArithmeticExpr (e1, e2) ->
        let t1 = validateExpr (fst e1)
        let t2 = validateExpr e2
        if fst t1 && fst t2 then
            (snd t1 = snd t2, snd t1)
        else (false, Type(Void, None))
    | UnaryExpr (_, e) -> validateExpr e
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
                                    try snd (validateExpr expr) = ((fst(functions[iden]))[exprs |> List.findIndex (fun e -> e = expr)]).Value
                                    with | _ -> true)
                (callValid, (snd functions[iden]))
            else (true, unionValues[iden])
    | ArrayExpr (iden, _) ->
        if variables.ContainsKey(iden) |> not && constants.ContainsKey(iden) |> not then
            printfn $"{iden} does not exist"; (false, Type(Void, None))
        else (true, if variables.ContainsKey(iden) then (fst variables[iden].Value, None) else (fst constants[iden].Value, None))
    | DataAccessExpr (iden, mem) ->
        if (variables.ContainsKey(iden) || constants.ContainsKey(iden) || activeComponents.Contains(iden)) |> not then
            printfn $"{iden} does not exist"; (false, Type(Void, None))
        else
            let newMem = match mem with
                         | DataAccessExpr (i, _) -> i
                         | IdentifierExpr i -> i
                         | _ -> printfn "Cannot be a member"; ""
            
            if activeComponents.Contains(iden) then
                try let memType = components[iden].Value |> List.find (fun (name, _) -> name = newMem)
                    (true, snd memType)
                with | _ ->
                    printfn $"Could not find member {mem}"     
                    (false, Type(Void, None))
            else
                try let memType = customTypes[iden].Value |> List.find (fun (name, _) -> name = newMem)
                    (true, snd memType)
                with | _ ->
                    printfn $"Could not find member {mem}"     
                    (false, Type(Void, None))
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
                        | RecordLiteral (_, members) ->
                            let mutable ret = Type(Void, None)
                            for cusTyp in customTypes do
                                let typ = Seq.zip cusTyp.Value.Value members
                                          |> Seq.forall (fun (c, m) -> fst c = fst m && snd c = snd (validateExpr (snd m)))
                                if typ then ret <- Type(Custom cusTyp.Key, None)
                            ret
                        | _ -> Type (Void, None)
        (true, litToType)
    | _ -> (true, Type(Void, None))
    
let rec validateStatement statement =
    match statement with
    | Binding bind ->
        match bind with
        | ImmutableBinding immut ->
            let valid, typ = snd immut |> validateExpr
            checkType typ
            if valid then constants.Add(fst (fst immut), Some typ)
        | MutableBinding mut ->
            let valid, typ = snd mut |> validateExpr
            checkType typ
            if valid then variables.Add(fst (fst mut), Some typ)
        | FunctionDeclaration func ->
            let param = [ for x in snd (fst (fst func)) do
                                 match x with
                                 | Unspecified iden ->
                                     constants.Add(iden, Type(Void, None) |> Some)
                                     yield None
                                 | Specified ((_, iden), typOpt) ->
                                     checkType typOpt.Value
                                     constants.Add(iden, typOpt)
                                     yield typOpt
                        ]
            for e in snd func do validateStatement e
            let retType = match snd (fst func) with
                          | Some ret -> ret
                          | None ->
                              match (snd func)[(snd func).Length - 1] with
                              | Statement.Expression e ->
                                  match e with
                                  | Expression e -> validateExpr e |> snd
                                  | _ -> printfn "Must end function with expression"; Type(Void, None)
                              | _ -> printfn "Must end function with expression"; Type(Void, None)
            
            functions.Add(fst (fst (fst func)), (Seq.toList param, retType))
            for x in snd(fst (fst func)) do
                match x with
                | Unspecified iden ->
                    constants.Remove(iden) |> ignore
                | Specified ((_, iden), _) ->
                    constants.Remove(iden) |> ignore
        | Reassignment res ->
            let varType = match fst (fst res) with
                          | IdentifierExpr s ->
                                if variables.ContainsKey(s) |> not then
                                    if constants.ContainsKey(s) then
                                        printfn $"{s} is not mutable"
                                    else printfn $"{s} does not exist"
                                    Type(Void, None)
                                else variables[s].Value
                          | DataAccessExpr(s, _) ->
                                if activeComponents.Contains(s) |> not &&  variables.ContainsKey(s) |> not then
                                    if constants.ContainsKey(s) then
                                        printfn $"{s} is not mutable"
                                    else printfn $"{s} does not exist"
                                    Type(Void, None)
                                else
                                    fst (fst res) |> validateExpr |> snd
                          | _ -> Type(Void, None)
            
            let value = snd res |> validateExpr |> snd
            if value <> varType then printfn $"Type {fst value} is not equal to {fst varType}"
        | EntityBinding (_, coms) ->
            coms |> List.iter (fun (iden, data) -> if components.ContainsKey(iden) |> not then printfn $"Component {iden} does not exist" )
    | Statement.Expression expr ->
        match expr with
        | ForExpr ((((_, iden), e), where), exprs) ->
            constants.Add(iden, snd(validateExpr e) |> Some)
            if where.IsSome then validateExpr where.Value |> ignore
            for e in exprs do validateStatement e
            constants.Remove(iden) |> ignore
        | WhileExpr (expr, exprs) ->
            validateExpr expr |> ignore
            for e in exprs do validateStatement e
        | IfExpr ((e, el), els) ->
            let evalIf ifExpr =
                match fst (fst ifExpr) with
                | Expr ex -> validateExpr ex |> ignore
                | LetStatement iden ->
                    if constants.ContainsKey(iden) then constants[iden] <- None
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
        | Expression e -> validateExpr e |> ignore
    | TypeDeclaration decl ->
        match decl with
        | RecordDeclaration (iden, members) ->
            let mems = members
                       |> List.map (fun ((_, iden), typ) -> (iden, typ))
            customTypes.Add(iden, Some mems)
        | UnionDeclaration (iden, cases) ->
            customTypes.Add(iden, None)
            for c in cases do
                match c with
                | Single s -> unionValues.Add(s, (Custom iden, None))
                | Multiple (s, _) -> unionValues.Add(s, (Custom iden, None))
        | ComponentDeclaration (iden, members) ->
            components.Add(iden, Some members)
            for _, typ in members do
                checkType typ
        | SystemDeclaration ((coms, _), exprs) ->
            coms |> List.iter (fun c ->
                if components.ContainsKey(c) |> not then printfn $"Component {c} does not exist"
                else activeComponents.Add(c))
            for e in exprs do
                validateStatement e
            for c in coms do activeComponents.Remove(c) |> ignore
        | TypeAlias tuple -> failwith "todo"
        | Extension tuple -> failwith "todo"

let analyze ast =
    match ast with
    | Success (a, _) ->
        for statement in a do
            validateStatement statement
    | Failure _ -> ()
    
    // for i in constants do printf $"{i}\t"
    //printfn ""
    //for i in functions do printf $"{i}\t"