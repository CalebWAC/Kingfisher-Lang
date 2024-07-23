open ParserLibrary.Core
open ParserLibrary.Std

// let input = "while i = 8 do i + 8"
let input = "for i in arr do { arr[i] + i }"

runString expression input
|> printResult