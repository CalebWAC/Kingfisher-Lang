open ParserLibrary.Core
open ParserLibrary.Std

let input =
    "fun print e = { println 'The element is: ${e}' }
     let arr = [0, 3, 5, 10]
     for i in arr do { print arr[i] }"

let ast = runString parseProgram input

ast
|> printResult

SemanticAnalysis.analyzeBindings ast
