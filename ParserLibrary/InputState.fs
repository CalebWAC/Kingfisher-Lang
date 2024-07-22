    // Line position and column tracking
module ParserLibrary.InputState

open System

type Position = { line : int; column : int }
type InputState = { lines: string array; position: Position }

let initialPos = { line = 0; column = 0 }
let incrCol pos = { pos with column = pos.column + 1 }
let incrLine pos = { line = pos.line + 1; column = 0 }

let fromStr str =
    if String.IsNullOrEmpty str then { lines = [||]; position = initialPos }
    else
        let separators = [| "\r\n"; "\n" |]
        let lines = str.Split(separators, StringSplitOptions.None)
        { lines = lines; position = initialPos }
        
let currentLine inputState =
    let linePos = inputState.position.line
    if linePos < inputState.lines.Length then inputState.lines[linePos]
    else "End of file"

let nextChar input =
    let line = input.position.line
    let col = input.position.column
    
    if line >= input.lines.Length then input, None
    else
        let current = currentLine input
        if col < current.Length then
            let char = current[col]
            let state = { input with position = incrCol input.position }
            state, Some char
        else
            let state = { input with position = incrLine input.position }
            state, Some '\n'
            
let rec readAllChars input =
    [
        let remainingInput, charOpt = nextChar input
        match charOpt with
        | None -> ()
        | Some char ->
            yield char
            yield! readAllChars remainingInput
    ]
    