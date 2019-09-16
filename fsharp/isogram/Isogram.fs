module Isogram

    open System

    let isIsogram (str:string):bool =
        let characters:char array =
            str.ToLower().ToCharArray()
            |> Array.filter (Char.IsLetter)
        let length:int =
            characters
            |> Array.length
        let distinctLength:int =
            characters
            |> Array.distinct
            |> Array.length
        length = distinctLength