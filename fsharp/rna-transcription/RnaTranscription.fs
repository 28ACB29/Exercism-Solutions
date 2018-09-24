module RnaTranscription

open System

let transciber (nucleotide:char):char =
    match nucleotide with
    | 'G' -> 'C'
    | 'C' -> 'G'
    | 'T' -> 'A'
    | 'A' -> 'U'

let toRna (dna: string): string =
    dna.ToCharArray()
    |> Array.map transciber
    |> String