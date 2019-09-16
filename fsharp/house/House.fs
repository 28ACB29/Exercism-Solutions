module House

    let private toThing (day:int):string =
        match day with
        | 1 -> "the house that Jack built."
        | 2 -> "the malt that lay in"
        | 3 -> "the rat that ate"
        | 4 -> "the cat that killed"
        | 5 -> "the dog that worried"
        | 6 -> "the cow with the crumpled horn that tossed"
        | 7 -> "the maiden all forlorn that milked"
        | 8 -> "the man all tattered and torn that kissed"
        | 9 -> "the priest all shaven and shorn that married"
        | 10 -> "the rooster that crowed in the morn that woke"
        | 11 -> "the farmer sowing his corn that kept"
        | 12 -> "the horse and the hound and the horn that belonged to"
        | _ -> ""

    let private cons (head:'a) (tail:'a list):'a list = head::tail

    let private createSong (verse:int):string =
        List.init verse (fun (i:int) -> toThing (verse - i))
        |> cons "This is"
        |> String.concat " "

    let recite (startVerse:int) (endVerse:int):string list =
        [startVerse..endVerse]
        |> List.map createSong