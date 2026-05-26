module PasswordChecker

open System

type PasswordError =
    | LessThan12Characters
    | MissingUppercaseLetter
    | MissingLowercaseLetter
    | MissingDigit
    | MissingSymbol

let (|LessThan12CharactersLong|_|) (s: string) =
    match s.Length < 12 with
    | true -> Some()
    | false -> None

let (|MissingUppercases|_|) s =
    match s |> Seq.exists Char.IsUpper with
    | false -> Some()
    | true -> None

let (|MissingLowercases|_|) s =
    match s |> Seq.exists Char.IsLower with
    | false -> Some()
    | true -> None

let (|MissingDigits|_|) s =
    match s |> Seq.exists Char.IsDigit with
    | false -> Some()
    | true -> None

let (|MissingSymbols|_|) s =
    match s |> Seq.exists (fun c -> not (Char.IsLetter c) && not (Char.IsDigit c)) with
    | false -> Some()
    | true -> None


/// Validate the given password against the rules defined in the instructions. If it meets all
/// of the rules, return a result indicating success; otherwise return a result indicating
/// failure and an error indicating which rule was violated.
let checkPassword (password: string) : Result<string, PasswordError> =
    match password with
    | LessThan12CharactersLong -> Error LessThan12Characters
    | MissingUppercases -> Error MissingUppercaseLetter
    | MissingLowercases -> Error MissingLowercaseLetter
    | MissingDigits -> Error MissingDigit
    | MissingSymbols -> Error MissingSymbol
    | _ -> Ok password

/// Return a human-readable message indicating the meaning of the given result value.
let getStatusMessage (result: Result<string, PasswordError>) : string =
    match result with
    | Ok _ -> "OK"
    | Error LessThan12Characters -> "Error: does not have at least 12 characters"
    | Error MissingUppercaseLetter -> "Error: does not contain at least one uppercase letter"
    | Error MissingLowercaseLetter -> "Error: does not contain at least one lowercase letter"
    | Error MissingDigit -> "Error: does not contain at least one digit"
    | Error MissingSymbol -> "Error: does not contain at least one symbol"