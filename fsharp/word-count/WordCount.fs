module WordCount

open System

let private removePunctuation (input:string):string =
    input.ToCharArray()
    |> Array.filter (fun (character: char) -> Char.IsLetterOrDigit(character))
    |> String

let countWords (phrase:string):Map<string, int> =
    phrase.Split[|' '|]
    |> Array.map (fun (word:string) -> removePunctuation word)
    |> Array.countBy id
    |> Map.ofArray