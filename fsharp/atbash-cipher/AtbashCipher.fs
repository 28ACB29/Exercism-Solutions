module AtbashCipher

open System

let private forward:char array = [|'a'..'z'|]

let private reverse:char array = Array.rev forward

let private encodeChar (character:char) =
    match System.Char.IsLetter(character) with
    | true -> reverse.[Array.findIndex (fun (element: char) -> element = character) forward]
    | false -> character

let private decodeChar (character:char) =
    match System.Char.IsLetter(character) with
    | true -> forward.[Array.findIndex (fun (element: char) -> element = character) reverse]
    | false -> character

let encode (str:string):string =
    str.ToLower().ToCharArray()
    |> Array.map encodeChar
    |> Array.chunkBySize 5
    |> Array.map (String)
    |> String.concat " "

let decode (str:string):string =
    str.Replace(" ", "").ToCharArray()
    |> Array.map decodeChar
    |> String