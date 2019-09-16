module Raindrops

    let private dictionary:(int * string) array = [|(3, "Pling"); (5, "Plang"); (7, "Plong")|]

    let private appender (number:int) (buffer:string) (factor:int, word:string) =
        match number % factor with
        | 0 -> buffer + word
        | _ -> buffer

    let private sentenceCreator (number:int):string =
        dictionary
        |> Array.fold (appender number) ""

    let convert (number:int):string =
        let sentence:string = sentenceCreator number
        match sentence with
        | "" ->
            number
            |> string
        | _ -> sentence
