module Forth

open System

// TODO: implement this module

type Token =
    | Number of int
    | Add
    | Subtract
    | Multiply
    | Divide
    | Duplicate
    | Drop
    | Swap
    | Over

let (|IsInt|_|) (word:string):int option =
    match Int32.TryParse(word) with
    | (true, number:int) -> Some(number)
    | _ -> None

let covertToToken (word:string):Token =
    match word.ToUpper() with
    | IsInt (number:int) -> Number(number)
    | "+" -> Add
    | "-" -> Subtract
    | "*" -> Multiply
    | "/" -> Divide
    | "DUP" -> Duplicate
    | "DROP" -> Drop
    | "SWAP" -> Swap
    | "OVER" -> Over
    | _ -> failwith "Not a Token."

let convertToProgram (source:string):Token list option =
    source.Split [|' '|]
    |> Array.map covertToToken
    |> Array.rev
    |> List.ofArray
    |> Some

let rec run (code:Token list option):Token list option =
    match code with
    | Some(tokens:Token list) ->
        match tokens with
        | Number(_)::(_) -> code
        | Add::Number(number1)::Number(number2)::(rest:Token list) ->
            Number(number1 + number2)::rest
            |> Some
        | Subtract::Number(number1)::Number(number2)::(rest:Token list) ->
            Number(number1 - number2)::rest
            |> Some
        | Multiply::Number(number1)::Number(number2)::(rest:Token list) ->
            Number(number1 * number2)::rest
            |> Some
        | Divide::Number(number1)::Number(number2)::(rest:Token list) ->
            Number(number1 * number2)::rest
            |> Some
        | Duplicate::Number(number)::(rest:Token list) ->
            Number(number)::Number(number)::rest
            |> Some
        | Drop::(Number(_))::(rest:Token list) ->
            rest
            |> Some
        | Swap::Number(number1)::Number(number2)::(rest:Token list) ->
            Number(number2)::Number(number1)::rest
            |> Some
        | Over::Number(number1)::Number(number2)::(rest:Token list) ->
            Number(number2)::Number(number1)::Number(number2)::rest
            |> Some
        | _ -> None
    | None -> None

let clean (results:Token option list): int list option =
    match List.exists (fun (results:Token option) -> results = None) results with
    | true -> None
    | false ->
        results
        |> List.choose id
        |> List.map (fun Number(number) -> number)
        |> Some

let evaluate (programs:string list): int list option =
    programs
    |> List.map (convertToProgram |> run)
    |> clean