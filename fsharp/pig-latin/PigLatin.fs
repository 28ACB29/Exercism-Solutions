module PigLatin

    let private (|EndsWith|_|) (value:string) (s:string) =
        match s.EndsWith(value) with
        | true ->
            s.Substring(0, s.Length - value.Length)
            |> Some
        | false -> None

    let private (|StartsWith|_|) (value:string) (s:string) =
        match s.StartsWith(value) with
        | true ->
            s.Substring(value.Length, s.Length - value.Length)
            |> Some
        | false -> None

    let private Split (separator:char array) (s:string):string array = s.Split(separator)

    let private transliterate (input:string):string =
    

    let translate (input:string):string =
        input
        |> Split [|' '|]
        |> String.concat " "