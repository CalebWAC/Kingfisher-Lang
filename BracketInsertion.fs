module BracketInsertion

let insertBrackets (program : string) =
    let openings =
        (" ", program)
        ||> Seq.fold (fun prev curr ->
            if (prev[prev.Length - 1] = 'd' && curr = 'o') ||
               (prev[prev.Length - 1] = 'i' && curr = 's') ||
               (prev[prev.Length - 1] = ':' && curr = '=')
            then prev + curr.ToString() + " {"
            else try
                     if (prev[prev.Length - 3..prev.Length - 1] = "the" && curr = 'n') ||
                        (prev[prev.Length - 3..prev.Length - 1] = "els" && curr = 'e')
                     then prev + curr.ToString() + " {"
                     else prev + curr.ToString()
                 with | _ -> prev + curr.ToString()
        )
    
    let lines = Array.concat [openings.Split([| "\r\n"; "\n" |], System.StringSplitOptions.None); [|""|]]
    
    let mutable tabs = 0
    for i in 0..lines.Length - 1 do
        let newTabs =
            let mutable spaces = 0
            let mutable beginning = true
            for j in 0..lines[i].Length - 1 do
                if lines[i][j] = ' ' && beginning then spaces <- spaces + 1
                else beginning <- false
            spaces / 4
             
        for _ in newTabs..tabs - 1 do
            lines[i] <- lines[i] + "}"
        
        tabs <- newTabs
    
    lines
    |> Seq.map (fun line -> line + "\n")
    |> Seq.reduce (+)