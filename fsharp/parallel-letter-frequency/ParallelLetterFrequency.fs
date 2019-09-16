module ParallelLetterFrequency

    open System

    let frequency (texts:string list):Map<char, int> =
        texts
        |> String.concat ""
        |> (fun (chainedTexts:string) -> chainedTexts.ToCharArray())
        |> Array.filter Char.IsLetter
        |> Array.map Char.ToLower
        |> Array.countBy id
        |> Map.ofArray