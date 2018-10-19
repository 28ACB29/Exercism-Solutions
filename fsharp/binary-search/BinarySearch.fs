module BinarySearch

let rec find (input:'a array) (value:'a):'a option =
    match input.Length with
    | 0 -> None
    | 1 ->
        match input.[0] = value with
        | true -> value |> Some
        | false -> None
    | _ ->
        let middleIndex:int = input.Length / 2
        match input.[middleIndex] = value with
        | true -> value |> Some
        | false ->
            match input.[middleIndex] < value with
            | true -> find input.[..middleIndex - 1] value
            | false -> find input.[middleIndex..] value