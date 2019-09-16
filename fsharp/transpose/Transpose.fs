module Transpose

open System

let realTranspose (table:char array array):char array array =
    let rows:int = table.Length
    match rows with
    | 0 -> [||]
    | _ ->
        let columns:int array =
            table
            |> Array.map (fun (row:char array) -> row.Length)
        let maxColumns:int =
            columns
            |> Array.max
        Array.init maxColumns (fun (i:int) -> Array.init rows (fun (j:int) -> if j < columns.[i] then table.[j].[i] else ' '))

let rightTrim (letters:char array):char array =
    let lastIndex:int =
        letters
        |> Array.findIndexBack (fun (letter:char) -> letter |> Char.IsWhiteSpace |> not)
    letters.[..lastIndex]

let transpose (input:string list):string list =
    input
    |> Array.ofList
    |> Array.map (fun (line:string) -> line.ToCharArray())
    |> realTranspose
    |> Array.map (fun (row:char array) -> row |> rightTrim |> String)
    |> List.ofArray