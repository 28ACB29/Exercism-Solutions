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

let rec private createAminoAcids (codons:char array list):string list =
    match codons with
    | [] -> []
    | (head:char array)::(tail:char array list) ->
        match RNATranslation head with
        | Methionine -> "Methionine"::(createAminoAcids tail)
        | Phenylalanine -> "Phenylalanine"::(createAminoAcids tail)
        | Leucine -> "Leucine"::(createAminoAcids tail)
        | Serine -> "Serine"::(createAminoAcids tail)
        | Tyrosine -> "Tyrosine"::(createAminoAcids tail)
        | Cysteine -> "Cysteine"::(createAminoAcids tail)
        | Tryptophan -> "Tryptophan"::(createAminoAcids tail)
        | STOP -> []

let proteins (rna:string):string list =
    rna.ToCharArray()
    |> Array.chunkBySize 3
    |> List.ofArray
    |> createAminoAcids