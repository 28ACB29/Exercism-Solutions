module NthPrime

    let (|LessThan|_|) (a:'a when 'a:comparison) (b:'a when 'a:comparison) =
        match b < a with
        | true ->
            ()
            |> Some
        | false -> None

    let prime (nth:int):int option =
        match nth with
        | LessThan 1 -> None
        | _ -> 