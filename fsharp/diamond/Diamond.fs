module Diamond

open System

// TODO: implement this module

let private letters:char array = [|'A'..'Z'|]

let private createRow (length:int) (index:int):string =
    Array.init (2 * length + 1) (fun (i:int) -> if i = (length - index) || i = (length + index) then letters.[index] else ' ')
    |> String

let make (letter:char):string =
    let index:int = Array.findIndex (fun (element:char) -> element = letter) letters
    let length:int = 2 * index + 1
    Array.init length (fun (i:int) -> createRow index (index - Math.Abs(i - index)))
    |> String.concat "\n"