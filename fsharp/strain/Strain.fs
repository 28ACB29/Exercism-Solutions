module Seq

let rec keep (pred:'a -> bool) (xs:'a seq):'a seq = seq{for x in xs do if x |> pred then yield x}

let rec discard (pred:'a -> bool) (xs:'a seq):'a seq = seq{for x in xs do if x |> pred |> not then yield x}