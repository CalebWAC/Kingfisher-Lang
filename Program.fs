open ParserLibrary.Core
open ParserLibrary.Std

let input = "let val = if 6 < 7 then { 7 + 9 } else { num }"

runString immutableBinding input
|> printResult