module ProteinTranslation

    type private RNASequence =
        | Methionine
        | Phenylalanine
        | Leucine
        | Serine
        | Tyrosine
        | Cysteine
        | Tryptophan
        | STOP

    let private RNATranslation (codon:char array) =
        match codon with
        | [|'A'; 'U'; 'G'|]
            -> Methionine
        | [|'U'; 'U'; 'U'|]
        | [|'U'; 'U'; 'C'|]
            -> Phenylalanine
        | [|'U'; 'U'; 'A'|]
        | [|'U'; 'U'; 'G'|]
            -> Leucine
        | [|'U'; 'C'; 'U'|]
        | [|'U'; 'C'; 'C'|]
        | [|'U'; 'C'; 'A'|]
        | [|'U'; 'C'; 'G'|]
            -> Serine
        | [|'U'; 'A'; 'U'|]
        | [|'U'; 'A'; 'C'|]
            -> Tyrosine
        | [|'U'; 'G'; 'U'|]
        | [|'U'; 'G'; 'C'|]
            -> Cysteine
        | [|'U'; 'G'; 'G'|]
            -> Tryptophan
        | [|'U'; 'A'; 'A'|]
        | [|'U'; 'A'; 'G'|]
        | [|'U'; 'G'; 'A'|]
            -> STOP
        | _ -> failwith "Not a valid codon."

    let private toString (rnaSequence:RNASequence):string =
        match rnaSequence with
            | Methionine -> "Methionine"
            | Phenylalanine -> "Phenylalanine"
            | Leucine -> "Leucine"
            | Serine -> "Serine"
            | Tyrosine -> "Tyrosine"
            | Cysteine -> "Cysteine"
            | Tryptophan -> "Tryptophan"
            | STOP -> ""

    let proteins (rna:string):string list =
        rna.ToCharArray()
        |> Array.chunkBySize 3
        |> List.ofArray
        |> List.map RNATranslation
        |> List.takeWhile ((<>) STOP)
        |> List.map toString