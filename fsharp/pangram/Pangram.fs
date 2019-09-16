module Pangram

    open System

    let private alphabet:char array = [|'a'..'z'|]

    let isPangram (input:string):bool =
        input.ToCharArray()
        |> Array.distinct
        |> Array.filter (Char.IsLetter)
        |> Array.map (Char.ToLower)
        |> Array.sort
        |> (=) alphabet