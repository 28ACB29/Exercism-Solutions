module Isogram

let private alphabet:char array = [|'a'..'z'|]

let isIsogram (str:string):bool =
    let characters:char array =
        str.ToLower().ToCharArray()
        |> Array.filter (fun (letter:char) -> Array.contains letter alphabet)
    let length:int =
        characters
        |> Array.length
    let distinctLength:int =
        characters
        |> Array.distinct
        |> Array.length
    length = distinctLength