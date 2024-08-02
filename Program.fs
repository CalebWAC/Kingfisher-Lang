open System.IO
open BracketInsertion
open ParserLibrary.Core
open ParserLibrary.Std
open System.Diagnostics

[<EntryPoint>]
let main loc =
    use reader = new StreamReader(loc[0])
    let file = reader.ReadToEnd()
    
    let program = insertBrackets file
    
    let ast = runString parseProgram program

    //ast
    //|> printResult

    SemanticAnalysis.analyze ast
    CodeGeneration.generate ast
    
    //printfn "\n\n"
    let mutable startInfo = ProcessStartInfo()
    startInfo.FileName <- "cmd.exe"
    startInfo.WorkingDirectory <- "F:\Kingfisher Language\ECS"
    startInfo.Arguments <- "/C haxe --main Main.hx --interp"
    Process.Start(startInfo) |> ignore
    
    0