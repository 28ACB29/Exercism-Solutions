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
        // i = column index, j = original row index -> check column exists in that row (i < columns.[j])
        Array.init maxColumns (fun (i:int) -> Array.init rows (fun (j:int) -> if i < columns.[j] then table.[j].[i] else ' '))

let rightTrim (letters:char array):char array =
    // If there is no non-whitespace char, return empty array instead of calling findIndexBack (which would throw)
    match letters |> Array.tryFindIndexBack (fun (letter:char) -> letter |> Char.IsWhiteSpace |> not) with
    | None -> [||]
    | Some(lastIndex:int) -> letters.[..lastIndex]

let transpose (input:string list):string list =
    input
    |> Array.ofList
    |> Array.map (fun (line:string) -> line.ToCharArray())
    |> realTranspose
    |> Array.map (fun (row:char array) -> row |> rightTrim |> String)
    |> List.ofArray