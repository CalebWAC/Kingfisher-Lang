open ParserLibrary.Core
open ParserLibrary.Std

let input = "let var: float array = 18.9"

runString immutableBinding input
|> printResult