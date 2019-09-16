module RnaTranscription

    open System

    let private transciber (nucleotide:char):char =
        match nucleotide with
        | 'G' -> 'C'
        | 'C' -> 'G'
        | 'T' -> 'A'
        | 'A' -> 'U'
        | _ -> failwith "Invalid Nucleotide"

    let toRna (dna: string):string =
        dna.ToCharArray()
        |> Array.map transciber
        |> String