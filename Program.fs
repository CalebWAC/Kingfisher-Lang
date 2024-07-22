open ParserLibrary.Core
open ParserLibrary.Std

let input = "let index = -Transform@entity"

runString immutableBinding input
|> printResult