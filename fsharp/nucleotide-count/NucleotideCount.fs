module NucleotideCount

let private nucleotides = [|'A'; 'C'; 'G'; 'T'|]

let private onlyNucleotides (strand:string):bool =
    strand.ToCharArray()
    |> Array.forall (fun (character:char) -> Array.contains character nucleotides)

let private emptyCount:Option<Map<char, int>> =
    [|('A', 0); ('C', 0); ('G', 0); ('T', 0)|]
    |> Map.ofArray
    |> Some

let nucleotideCounts (strand:string):Option<Map<char, int>> =
    match onlyNucleotides strand with
    | true ->
        match strand with
        | "" -> emptyCount
        | _ ->
            strand.ToCharArray()
            |> Array.countBy id
            |> Map.ofArray
            |> Some
    | false -> None
