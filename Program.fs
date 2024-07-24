open ParserLibrary.Core
open ParserLibrary.Std

let input =
    "fun print e = { println 'The element is: ${e}' }
     for i in arr do { nonExistentVariable }
     let arr = invalid"

let ast = runString parseProgram input

ast
|> printResult

SemanticAnalysis.analyzeBindings ast
