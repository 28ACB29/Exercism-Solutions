module Pangram

open System

let private letters:char array = [|'a'..'z'|]

let isPangram (input:string):bool =
    input.ToLower().ToCharArray()
    |> Array.distinct
    |> Array.filter (fun (character:char) -> character |> Char.IsLetter)
    |> Array.sort
    |> (=) letters