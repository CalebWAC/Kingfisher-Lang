open ParserLibrary.Core
open ParserLibrary.Std

let input = "1827   -3.l94"
let parser = intLit .>> whitespace .>>. floatLit

runString parser input
|> printResult