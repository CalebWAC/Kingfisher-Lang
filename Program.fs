open ParserLibrary.Core
open ParserLibrary.Std

// let input = "while i = 8 do i + 8"
let input = "while 8 = i do { i + 8 }"

runString whileExpr input
|> printResult