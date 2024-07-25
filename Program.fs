﻿open ParserLibrary.Core
open ParserLibrary.Std

let input =
    "fun add (a: int) (b : int) = { a + b }
     let num1 = 5
     let num2 = 6
     let num3 = add num1 num2"

let ast = runString parseProgram input

ast
|> printResult

SemanticAnalysis.analyzeBindings ast
