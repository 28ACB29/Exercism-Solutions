module PigLatin

    let private (|Empty|_|) (s:string) =
        match s with
        | "" -> Some()
        | _ -> None

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

    let private (|Vowel|_|) (ch:char) =
        match System.Char.ToLower ch with
        | 'a'
        | 'e'
        | 'i'
        | 'o'
        | 'u' -> Some()
        | _ -> None

    let private Split (separator:char array) (s:string):string array = s.Split(separator)

    let private transliterate (input:string):string =
        let isVowel (ch:char) (pos:int) =
            match System.Char.ToLower ch with
            | 'a'
            | 'e'
            | 'i'
            | 'o'
            | 'u' -> true
            | 'y' when pos > 0 -> true
            | _ -> false

        match input with
        | Empty -> ""
        | StartsWith "xr" _ -> input + "ay"
        | StartsWith "yt" _ -> input + "ay"
        | _ ->
            let rec findCluster (index:int):int =
                match index >= input.Length with
                | true -> index
                | false ->
                    match input.[index] with
                    | 'q' when index + 1 < input.Length && System.Char.ToLower input.[index + 1] = 'u' ->
                        // include 'qu' in the consonant cluster
                        findCluster (index + 2)
                    | Vowel -> index
                    | _ -> findCluster (index + 1)
            let cut:int = findCluster 0
            let head:string = input.Substring(0, cut)
            let tail:string = if cut < input.Length then input.Substring(cut) else ""
            tail + head + "ay"

    let translate (input:string):string =
        input
        |> Split [|' '|]
        |> Array.map transliterate
        |> String.concat " "