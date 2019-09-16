module Acronym

    open System

    let abbreviate (phrase:string):string =
        phrase.Split [|' '; '-'|]
        |> Array.filter (fun (word:string) -> word.Length > 0)
        |> Array.map (fun (word:string) -> word.ToCharArray().[0] |> Char.ToUpper)
        |> String