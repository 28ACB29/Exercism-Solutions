module Raindrops

let private dictionary:(int * string) array = [|(3, "Pling"); (5, "Plang"); (7, "Plong")|]

let convert (number:int):string =
    let appender (buffer:string) (factor:int, word:string) =
        match number % factor with
        | 0 -> buffer + word
        | _ -> buffer
    let sentence:string = Array.fold appender "" dictionary
    match sentence with
    | "" ->
        number
        |> string
    | _ -> sentence
