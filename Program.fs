open ParserLibrary.Core
open ParserLibrary.Std

let input =
    "let num1 : int = 5
     let num2 : int = 7
     num1 + num2 = 5 && true"

let ast = runString parseProgram input

ast
|> printResult

SemanticAnalysis.analyzeBindings ast
