module PythagoreanTriplet

    let createTuples (sum:int):(int * int * int) seq =
        seq{
        for c in 1..(sum - 2) do
            for b in 1..c do
                for a in 1..b do
                    if a + b + c = sum && (a * a) + (b * b) = (c * c) then
                        yield (a, b, c)}


    let tripletsWithSum (sum:int):(int * int * int) list =
        sum
        |> createTuples
        |> List.ofSeq