module CryptoSquare

open System

let extend (size:int) (filler:'a) (original:'a array):'a array =
    let length:int = original.Length
    match size > length with
    | true ->
        (fun _ -> filler)
        |> Array.init (size - length)
        |> Array.append original
    | false -> original

let ciphertext (input:string):string =
    match input with
    | "" -> ""
    | _ ->
        let trimmed:char array =
            input.ToLower().ToCharArray()
            |> Array.filter (fun (character:char) -> character |> Char.IsLetterOrDigit)
        let length:int = trimmed.Length
        match length with
        | 0 -> ""
        | _ ->
            let rows:int =
                length
                |> double
                |> Math.Sqrt
                |> Math.Floor
                |> int
            let columns:int =
                length
                |> double
                |> Math.Sqrt
                |> Math.Ceiling
                |> int
            trimmed
            |> extend (rows * columns) ' '
            |> Array.permute (fun (i:int) -> (i % rows) * columns + (i / rows))
            |> Array.chunkBySize rows
            |> Array.map (fun (row:char array) -> row |> String)
            |> String.concat " "