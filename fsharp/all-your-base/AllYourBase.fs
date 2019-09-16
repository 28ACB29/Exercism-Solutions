module AllYourBase

    let private (|GreaterThan|_|) (a:'a when 'a:comparison) (b:'a when 'a:comparison) =
        match b > a with
        | true ->
            ()
            |> Some
        | false -> None

    let private (|ValidDigits|_|) (inputBase:int) (digits:int list) =
        match List.forall (fun (digit:int) -> digit > -1 && digit < inputBase) digits with
        | true ->
            ()
            |> Some
        | false -> None

    let private digitsToNumber (inputBase:int) (digits:int list):int =
        match digits with
        | [] -> 0
        | _ ->
            digits
            |> List.fold (fun (number:int) (digit:int) -> number * inputBase + digit) 0

    let private numberTodigits (outputBase:int) (number:int):int list =
        let rec tailCall (digits:int list) (remainder:int):int list =
            match remainder with
            | 0 -> digits
            | _ -> tailCall ((remainder % outputBase)::digits) (remainder / outputBase)
        let digits:int list = tailCall [] number
        match digits with
        | [] -> [0]
        | _ -> digits

    let rebase (digits:int list) (inputBase:int) (outputBase:int):int list option =
        match inputBase with
        | GreaterThan 1 ->
            match outputBase with
            | GreaterThan 1 ->
                match digits with
                | ValidDigits inputBase ->
                    digits
                    |> digitsToNumber inputBase
                    |> numberTodigits outputBase
                    |> Some
                | _ -> None
            | _ -> None
        | _ -> None