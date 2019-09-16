module IsbnVerifier

    open System

    let private zero:int = int '0'

    let isbn13Prefix:char array = [|'9'; '7'; '8'|]

    let private (|CorrectDigits|_|) (digits:char array):(char array * char) option =
        let elements:char array = digits.[..digits.Length - 2]
        let checkDigit:char = digits.[digits.Length - 1]
        match Array.forall Char.IsDigit elements && (checkDigit |> Char.IsDigit || checkDigit = 'X') with
        | true ->
            (elements, checkDigit)
            |> Some
        | false -> None

    let private charToInt (digit:char):int = int digit - zero

    let private convertCheckDigit (digit:char):int =
        match digit with
        | 'X' -> 10
        | _ -> charToInt digit

    let private calculateCheckSum (elements:char array) (checkDigit:char):int =
        elements
        |> Array.mapi (fun (i:int) (letter:char) -> (charToInt letter) * (10 - i))
        |> Array.sum
        |> (+) (convertCheckDigit checkDigit)

    let private calculateCheckDigit (elements:char array):char =
        elements
        |> Array.mapi (fun (i:int) (letter:char) -> (charToInt letter) * 3 * (i % 2))
        |> Array.sum
        |> (fun (sum:int) -> 10 - (sum % 10))
        |> char

    let private (|CorrectDigits13|_|) (digits:char array):(char array * char) option =
        let elements:char array = digits.[..digits.Length - 2]
        let checkDigit:char = digits.[digits.Length - 1]
        match Array.forall Char.IsDigit elements && (checkDigit |> Char.IsDigit || checkDigit = 'X') with
        | true ->
            (elements, checkDigit)
            |> Some
        | false -> None

    let private calculateCheckSum13 (elements:char array) (checkDigit:char):int =
        elements
        |> Array.mapi (fun (i:int) (letter:char) -> (charToInt letter) * 3 * (i % 2))
        |> Array.sum
        |> (+) (convertCheckDigit checkDigit)

    let isValid (isbn:string):bool =
        let digits:char array =
            isbn.ToCharArray()
            |> Array.filter Char.IsLetterOrDigit
        match digits.Length = 10 with
        | true ->
            match digits with
            | CorrectDigits(elements:char array, checkDigit:char) -> (calculateCheckSum elements checkDigit) % 11 = 0
            | _ -> false
        | false -> false

    let covertISBN10ToISBN13 (isbn:string):string option =
        let digits:char array =
            isbn.ToCharArray()
            |> Array.filter Char.IsLetterOrDigit
        match digits.Length = 10 with
        | true ->
            match digits with
            | CorrectDigits(elements:char array, _) ->
                let newElements:char array = Array.append isbn13Prefix elements
                let newCheckDigit:char = calculateCheckDigit newElements
                Array.append newElements [|newCheckDigit|]
                |> String
                |> Some
            | _ -> None
        | false -> None

    let isValid13 (isbn:string):bool =
        let digits:char array =
            isbn.ToCharArray()
            |> Array.filter Char.IsLetterOrDigit
        match digits.Length = 13 with
        | true ->
            match digits with
            | CorrectDigits13(elements:char array, checkDigit:char) -> (calculateCheckSum13 elements checkDigit) % 10 = 0
            | _ -> false
        | false -> false