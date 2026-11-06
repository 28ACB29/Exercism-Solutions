module Acronym

    open System

    let getFirstLetter (word:string):char =
        word.ToCharArray()
        |> Array.filter (Char.IsLetter)
        |> Array.head
        |> Char.ToUpper

    let abbreviate (phrase:string):string =
        phrase.Split [|' '; '-'|]
        |> Array.filter (fun (word:string) -> word.Length > 0)
        |> Array.map getFirstLetter
        |> String