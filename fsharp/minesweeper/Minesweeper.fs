module Minesweeper

open System

let private unpack (rows:string list):char array array =
    rows
    |> Array.ofList
    |> Array.map (fun (row:string) -> row.ToCharArray())

let private isMine (A:char array array) (i:int) (j:int):int =
    match i, j with
    | i, j when i > -1 && i < A.Length && j > -1 && j < A.[0].Length ->
        match A.[i].[j] with
        | '*' -> 1
        | _ -> 0
    | _ -> 0

let private countMines (A:char array array) (i:int) (j:int):int =
    [|(i - 1, j - 1); (i - 1, j); (i - 1, j + 1); (i, j - 1); (i, j + 1); (i + 1, j - 1); (i + 1, j); (i + 1, j + 1)|]
    |> Array.sumBy (fun (row:int, column:int) -> isMine A row column)

let private mark (mines:int):char =
    match mines with
    | 1 -> '1'
    | 2 -> '2'
    | 3 -> '3'
    | 4 -> '4'
    | 5 -> '5'
    | 6 -> '6'
    | 7 -> '7'
    | 8 -> '8'
    | 9 -> '9'
    | _ -> ' '

let private processCell (A:char array array) (i:int) (j:int):char =
    match A.[i].[j] with
    | '*' -> '*'
    | _ ->
        countMines A i j
        |> mark


let private repack (board:char array array):string list =
    board
    |> Array.map (fun (row:char array) -> row |> String)
    |> List.ofArray

let annotate (input:string list):string list =
    let A =
        input
        |> unpack
    A
    |> Array.mapi (fun (i:int) (row:char array) -> row |> Array.mapi (fun (j:int) (cell:char) -> processCell A i j))
    |> repack