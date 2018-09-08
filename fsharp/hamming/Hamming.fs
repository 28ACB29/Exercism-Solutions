module Hamming

let private dissimilar (a: 'a) (b: 'a) =
    match a = b with
    | true -> 0
    | false -> 1

let distance (strand1: string) (strand2: string): int option =
    let bases1: char array = strand1.ToCharArray()
    let bases2: char array = strand2.ToCharArray()
    match bases1.Length = bases2.Length with 
    | true ->
        Array.fold2 (fun (distance: int) (base1: char) (base2: char) -> distance + dissimilar base1 base2) 0 bases1 bases2
        |> Some
    | false -> None