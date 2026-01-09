module AffineCipher

    open System

    let private letters:char array = [|'a'..'z'|]

    let private m:int = letters.Length

    let rec private gcd (a:int) (b:int):int =
        match a, b with
        | a, b when a = b -> a
        | _, 0 -> a
        | 0, _ -> b
        | a, b when a > b -> gcd (a % b) b
        | a, b when a < b -> gcd a (b % a)
        | _ -> 0

    let private inverse (a:int) (b:int):int =
        seq {1..(b - 1)}
        |> Seq.filter (fun (number:int) -> (a * number) % b = 1)
        |> Seq.head

    let private decrypt (a:int) (b:int) (letter:char):char =
        match Char.IsLetter letter with
        | true ->
            let index:int = Array.findIndex (fun (character:char) -> character = letter) letters
            letters.[(((inverse a m) * (index - b)) % m + m) % m]
        | false -> letter

    let encrypt (a:int) (b:int) (letter:char):char =
        match Char.IsLetter letter with
        | true ->
            let index:int = Array.findIndex (fun (character:char) -> character = letter) letters
            letters.[(a * index + b) % m]
        | false -> letter

    let decode (a:int) (b:int) (cipheredText:string):string =
        match gcd a m with
        | 1 ->
            cipheredText.ToCharArray()
            |> Array.filter Char.IsLetterOrDigit
            |> Array.map (decrypt a b)
            |> String
        | _ -> invalidArg "a" "Error: a and m must be coprime."


    let encode (a:int) (b:int) (plainText:string):string =
        match gcd a m with
        | 1 ->
            plainText.ToCharArray()
            |> Array.filter Char.IsLetterOrDigit
            |> Array.map (Char.ToLower >> encrypt a b)
            |> Array.chunkBySize 5
            |> Array.map String
            |> String.concat " "
        | _ -> invalidArg "a" "Error: a and m must be coprime."
