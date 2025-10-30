module RunLengthEncoding

    open System
    open System.Text

    let private yieldIfTrue (predicate:char -> bool) (character:char) =
        match predicate character with
        | true ->
            character
            |> Some
        | false -> None

    let (|Digit|_|) (character:char) =
        match Int32.TryParse(character.ToString()) with
        | (true, digit:int) -> Some(digit)
        | _ -> None

    let private encodeBack (element:'a) (accumulator:(int * 'a) list):(int * 'a) list =
        match element, accumulator with
        | element, (count:int, item:'a)::(next:(int * 'a) list) when element = item -> (count + 1, element)::next
        | _ -> (1, element)::accumulator

    let private realEncode (data:'a array):(int * 'a) list =
        Array.foldBack encodeBack data []

    let encode (input:string):string =
        input.ToCharArray()
        |> realEncode
        |> List.fold (fun (buffer:StringBuilder) (count: int, item: char) -> if count = 1 then buffer.Append(item) else buffer.Append(count).Append(item)) (StringBuilder())
        |> (fun (buffer:StringBuilder) -> buffer.ToString())

    let rec private decodeBack (element:char) (accumulator:(int * char) list):(int * char) list =
        match element with
        | Digit(digit:int) ->
            match accumulator with
            | (count:int, item:char)::(next:(int * char) list) ->
                match count with
                | 1 -> (digit, item)::next
                | _ -> (digit * 10 + count, item)::next
            | _ -> accumulator
        | _ -> (1, element)::accumulator

    let private realDecode (data:char array):char list =
        Array.foldBack decodeBack data []
        |> List.collect (fun (count: int, item: char) -> List.init count (fun _ -> item))

    let decode (input:string):string =
        input.ToCharArray()
        |> realDecode
        |> (fun (decoded:char list) -> String.Join("", decoded))