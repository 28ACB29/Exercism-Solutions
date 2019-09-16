module PerfectNumbers

    open System

    type Classification =
        | Perfect
        | Abundant
        | Deficient

    let private accumulateFactors (n:int) (accumulator:int) (number:int):int =
        match n % number with
        | 0 ->
            match number = n / number with
            | true -> accumulator + number
            | false -> accumulator + number + (n / number)
        | _ -> accumulator

    let private aliquotSum (n:int):int =
        match n with
        | 1 -> 0
        | _ ->
            let ceiling:int =
                n
                |> double
                |> Math.Sqrt
                |> int
            seq{2..ceiling}
            |> Seq.fold (accumulateFactors n) 1


    let classify (n:int):Classification option =
        match n with
        | n when n > 0 ->
            match n |> aliquotSum |> n.CompareTo |> Math.Sign with
            | -1 -> Abundant
            | 0 -> Perfect
            | 1 -> Deficient
            |> Some
        | _ -> None
