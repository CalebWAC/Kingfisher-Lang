open ParserLibrary.Core
open ParserLibrary.Std

let input =
    "let arr = [1, 2, 3, 4, 5]
     for i in arr do {
        if i % 2 = 0 then { println '${i}' }
     }"

let ast = runString parseProgram input

ast
|> printResult

SemanticAnalysis.analyzeBindings ast
