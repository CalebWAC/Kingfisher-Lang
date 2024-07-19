open Piglet.Parser

let configurator = ParserFactory.Configure<obj>()

let nonTerminal = configurator.CreateNonTerminal()