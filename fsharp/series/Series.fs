module Series

open System

let slices (str:string) (length:int):string list option =
    let characters:char array = str.ToCharArray()
    let size:int = characters.Length - length
    match length < 1 || size < 0 with
    | true -> None
    | false ->
        List.init (size + 1) (fun i -> characters.[i..(i + length - 1)] |> String)
        |> Some