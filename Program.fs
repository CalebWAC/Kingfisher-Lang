open ParserLibrary.Core
open ParserLibrary.Std

// let element = arr[0]\n
let input = "for i in arr do { 7 + 8\npop element }\n"

runString (statement()) input
|> printResult