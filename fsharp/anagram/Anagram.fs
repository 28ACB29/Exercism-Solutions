module Anagram

open System

let private stringSort (input:string) =
    input.ToCharArray()
    |> Array.sort
    |> String

let private isAnagram (source:string) (target:string) =
    let loweredSource:string = source.ToLower()
    let loweredTarget:string = target.ToLower()
    let sortedSource:string =
        loweredSource
        |> stringSort
    let sortedTarget:string =
        loweredTarget
        |> stringSort
    loweredSource = loweredTarget
    |> not
    |> (&&) (sortedSource = sortedTarget)

let findAnagrams (sources:string list) (target:string):string list =
    sources
    |> List.filter (fun (source:string) -> isAnagram source target)