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
    //CodeGeneration.generate ast
    
    //printfn "\n\n"
    //System.Diagnostics.Process.Start("cmd.exe", "/C haxe --main Main.hx --interp") |> ignore
    
    0
    
(*
type Vec3 = { var x: float, var y: float, var z: float }

com Transform = {
    position: Vec3
    rotation: Vec3
    scale: Vec3
}

ent player = Transform Player

sys Transform Player | Update = {
    Transform.position + 1
}
*)