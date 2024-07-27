open System.IO
open ParserLibrary.Core
open ParserLibrary.Std

[<EntryPoint>]
let main loc =
    use reader = new StreamReader(loc[0])
    let file = reader.ReadToEnd()
    
    let ast = runString parseProgram file

    ast
    |> printResult

    SemanticAnalysis.analyze ast
    CodeGeneration.generate ast
    
    printfn "\n\n\n\n"
    System.Diagnostics.Process.Start("cmd.exe", "/C haxe --main Main.hx --interp") |> ignore
    
    0