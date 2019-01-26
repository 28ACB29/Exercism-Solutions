module AllYourBase

let digitsToNumber (inputBase:int) (digits:int list) =
    match digits with
    | [] -> 0
    | _ ->
        digits
        |> List.fold (fun (number:int) (digit:int) -> number * inputBase + digit) 0

let numberTodigits (outputBase:int) (number:int) =
    let rec tailCall (digits:int list) (remainder:int):int list =
        match remainder with
        | 0 -> digits
        | _ -> tailCall ((remainder % outputBase)::digits) (remainder / outputBase)
    let digits:int list = tailCall [] number
    match digits with
    | [] -> [0]
    | _ -> digits

let rebase (digits:int list) (inputBase:int) (outputBase:int):int list option =
    match inputBase, outputBase with
    | inputBase, outputBase when inputBase > 1 && outputBase > 1 && List.forall (fun (digit:int) -> digit > -1 && digit < inputBase) digits ->
        digits
        |> digitsToNumber inputBase
        |> numberTodigits outputBase
        |> Some
    | _ -> None