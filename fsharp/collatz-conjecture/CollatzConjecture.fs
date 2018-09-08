module CollatzConjecture

let private nextNumber (n: int): int =
    match n % 2 with
    | 0 -> n / 2
    | 1 -> 3 * n + 1

let rec private countSteps (n: int, i: int): int * int =
    match n with
    | 1 -> (1, i)
    | _ -> countSteps (nextNumber n, i + 1)

let steps (number: int): int option =
    match number < 1 with
    | true -> None
    | false ->
        (number, 1)
        |> countSteps
        |> snd
        |> Some