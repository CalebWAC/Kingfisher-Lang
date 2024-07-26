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
    
    0
    
(* 
fun add (a: int) (b : int) = 
    { a + b }

let num1 = 5
let num2 = 6
let num3 = add num1 num2
*)
