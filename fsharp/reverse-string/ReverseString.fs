module ReverseString

open System

let reverse (input:string):string =
    input.ToCharArray()
    |> Array.rev
    |> String