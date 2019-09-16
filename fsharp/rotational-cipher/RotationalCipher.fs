module RotationalCipher

open System

let private lowers:char array = [|'a'..'z'|]

let private uppers:char array = [|'A'..'Z'|]

let private (|IsUpper|_|) (character:char) =
    match Char.IsUpper(character) with
    | true -> character |> Some
    | false -> None

let private (|IsLower|_|) (character:char) =
    match Char.IsLower(character) with
    | true -> character |> Some
    | false -> None

let private codec (shiftKey:int) (character:char) =
    match character with
    | IsLower(lower) -> lowers.[(Array.findIndex (fun (letter:char) -> letter = lower) lowers + shiftKey) % 26]
    | IsUpper(upper) -> uppers.[(Array.findIndex (fun (letter:char) -> letter = upper) uppers + shiftKey) % 26]
    | _ -> character

let rotate (shiftKey:int) (text:string) =
    text.ToCharArray()
    |> Array.map (fun (character:char) -> character |> codec shiftKey)