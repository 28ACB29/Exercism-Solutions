module PhoneNumber

    open System

    let private filter (input:string):string =
        input
        |> String.filter (fun (letter:char) -> not (Char.IsWhiteSpace(letter) || letter = '+' || letter = '-'  || letter = '.' || letter = '(' || letter = ')'))

    let private countDigits (input:string):Result<string,string> =
        match input.Length with
        | (length:int) when length < 10 -> Error "incorrect number of digits"
        | 10 -> Ok input
        | 11 ->
            match input.[0] with
            | '1' -> Ok input
            | _ -> Error "11 digits must start with 1"
        | _ -> Error "more than 11 digits"

    let private (|OnlyDigits|_|) (input:string) =
        match String.forall (Char.IsDigit) input with
        | true -> Some ()
        | false -> None

    let private (|HasLetter|_|) (input:string) =
        match String.exists (Char.IsLetter) input with
        | true -> Some ()
        | false -> None

    let private (|HasPunctuation|_|) (input:string) =
        match String.exists (Char.IsPunctuation) input with
        | true -> Some ()
        | false -> None
 
    let private validate (input:string):Result<string,string> =
        match input with
        | OnlyDigits -> Ok input
        | HasLetter -> Error "alphanumerics not permitted"
        | HasPunctuation -> Error "punctuations not permitted"
        | _ -> Error "unknown error"

    let private areaCode (input:string):Result<char,string> =
        match input.Length with
        | 10 -> Ok input.[0]
        | 11 -> Ok input.[1]
        | _ -> Error "incorrect number of digits"

    let private checkAreaCode (input:string):Result<string,string> =
        match areaCode input with
        | Ok(digit:char) ->
            match digit with
            | '0' -> Error "area code cannot start with zero"
            | '1' -> Error "area code cannot start with one"
            | '2'
            | '3'
            | '4'
            | '5'
            | '6'
            | '7'
            | '8'
            | '9' ->
                Ok input
            | _ -> Error "area code is not a digit"
        | Error(message:string) -> Error message

    let private exchangeCode (input:string):Result<char,string> =
        match input.Length with
        | 10 -> Ok input.[3]
        | 11 -> Ok input.[4]
        | _ -> Error "incorrect number of digits"

    let private checkExchangeCode (input:string):Result<string,string> =
        match exchangeCode input with
        | Ok(digit:char) ->
            match digit with
            | '0' -> Error "exchange code cannot start with zero"
            | '1' -> Error "exchange code cannot start with one"
            | '2'
            | '3'
            | '4'
            | '5'
            | '6'
            | '7'
            | '8'
            | '9' ->
                Ok input
            | _ -> Error "exchange code is not a digit"
        | Error(message:string) -> Error message

    let private parse (input:string):Result<uint64,string> =
        match UInt64.TryParse input with
        | (true, number:uint64) -> Ok number
        | _ -> Error "could not parse input"

    let clean (input:string):Result<uint64,string> =
        input
        |> filter
        |> countDigits
        |> Result.bind validate
        |> Result.bind checkAreaCode
        |> Result.bind checkExchangeCode
        |> Result.bind parse