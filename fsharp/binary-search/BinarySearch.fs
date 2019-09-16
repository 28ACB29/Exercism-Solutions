module BinarySearch

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

    let rec private binarySearch (input:'a array when 'a:comparison) (left:int) (right:int) (value:'a when 'a:comparison):int option =
        match right with
        | LessThan left -> None
        | _ ->
            let middleIndex:int = (left + right) / 2
            match input.[middleIndex] with
            | LessThan value -> binarySearch input (middleIndex + 1) (right) value
            | EqualTo value ->
                middleIndex
                |> Some
            | GreaterThan value -> binarySearch input (left) (middleIndex - 1) value

    let find (input:'a array when 'a:comparison) (value:'a when 'a:comparison):int option =
        match input with
        | [||] -> None
        | [|element:'a|] ->
            match element with
            | EqualTo value ->
                0
                |> Some
            | _ -> None
        | _ -> binarySearch input (0) (input.Length - 1) value