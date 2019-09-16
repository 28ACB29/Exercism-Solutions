module Luhn

    open System

    let private (|Valid|_|) (number:string) =
        let digits:char array =
            number.ToCharArray()
            |> Array.filter (not << Char.IsWhiteSpace)
        if digits.Length > 1 && Array.forall (Char.IsDigit) digits then
            digits
            |> Some
        else
            None

    let private zero:int = int '0'

    let private charToInt (digit:char):int = int digit - zero

    let private doubleFromRight (length:int) (i:int) (digit:int):int =
        match (length - i) % 2 with
        | 0 -> digit * 2
        | _ -> digit

    let private wrapDigit (digit:int):int =
        match digit > 9 with
        | true -> digit - 9
        | false -> digit

    let private mapDigit (length:int) (i:int) (digit:char):int =
        digit
        |> charToInt
        |> (doubleFromRight length i)
        |> wrapDigit

    let private mapDigits (digits:char array):int array =
        let length:int = digits.Length
        digits
        |> Array.mapi (mapDigit length)

    let private mod10 (digit:int):int = digit % 10

    let valid (number:string):bool =
        match number with
        | Valid(digits:char array) ->
            digits
            |> mapDigits
            |> Array.sum
            |> mod10
            |> (=) 0
        | _ -> false
