module Grains

    let square (n:int):Result<uint64,string> =
        match n > 0 && n < 65 with
        | true -> Ok(pown 2UL (n - 1))
        | false -> Error("square must be between 1 and 64")

    let total:Result<uint64,string> =
        seq{0..63}
        |> Seq.sumBy (fun (n:int) -> pown 2UL n)
        |> Ok