module LargestSeriesProduct

    open System

    let private (|LessThan|_|) (a:'a when 'a:comparison) (b:'a when 'a:comparison) =
        match b < a with
        | true ->
            ()
            |> Some
        | false -> None

    let private (|EqualTo|_|) (a:'a when 'a:comparison) (b:'a when 'a:comparison) =
        match b = a with
        | true ->
            ()
            |> Some
        | false -> None

    let private (|GreaterThan|_|) (a:'a when 'a:comparison) (b:'a when 'a:comparison) =
        match b > a with
        | true ->
            ()
            |> Some
        | false -> None

    let private (|ValidDigits|_|) (digits:char array) =
        match Array.forall (Char.IsDigit) digits with
        | true ->
            ()
            |> Some
        | false -> None

    let private zero:int =
        '0'
        |> int

    let toInt (character:char):int =
        character
        |> int
        |> (fun (digit:int) -> digit - zero)

    let largestProduct (input:string) (seriesLength:int):int option =
        let characters:char array = input.ToCharArray()
        let tooLong:int = characters.Length + 1
        match characters with
        | ValidDigits ->
            match seriesLength with
            | EqualTo 0 ->
                1
                |> Some
            | GreaterThan 0 & LessThan tooLong ->
                characters
                |> Array.windowed seriesLength
                |> Array.map (Array.map toInt >> Array.reduce (*))
                |> Array.max
                |> Some
            | _ -> None
        | _ -> None