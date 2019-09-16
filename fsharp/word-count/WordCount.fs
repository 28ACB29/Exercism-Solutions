module WordCount

    open System

    let private removePunctuation (input:string):string =
        input.ToCharArray()
        |> Array.filter Char.IsLetterOrDigit
        |> String

    let countWords (phrase:string):Map<string, int> =
        phrase.Split [|' '|]
        |> Array.map removePunctuation
        |> Array.countBy id
        |> Map.ofArray