module WordCount

let countWords (phrase:string):Map<string, int> =
    let removePunctuation (input:string):string =
        let filtered: char array =
            input.ToCharArray()
            |> Array.filter (fun (character: char) -> System.Char.IsLetterOrDigit(character))
        System.String(filtered)
    phrase.Split[|' '|]
    |> Array.map (fun (word:string) -> removePunctuation word)
    |> Array.countBy id
    |> Map.ofArray