module SumOfMultiples

let sum (numbers: int list) (upperBound: int): int =
    let generateMultiples (n: int): int list =
        let inclusiveNumber: int = upperBound / n
        let boundary: int =
            match upperBound % n = 0 with
            | true -> 1
            | false -> 0
        let exclusiveNumber: int = inclusiveNumber - boundary
        List.init exclusiveNumber (fun i -> (i + 1) * n)
    numbers
    |> List.collect generateMultiples
    |> List.distinct
    |> List.sum