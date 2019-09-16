module Series

    open System

    let slices (str:string) (length:int):string list option =
        let characters:char array = str.ToCharArray()
        let size:int = characters.Length - length
        match length < 1 || size < 0 with
        | true -> None
        | false ->
            characters
            |> Array.windowed length
            |> Array.map String
            |> List.ofArray
            |> Some