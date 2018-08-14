module Raindrops

let convert (number: int): string = 
    let dictionary: (int * string) array = [|(3, "Pling"); (5, "Plang"); (7, "Plong")|]
    let appender (buffer: string) (factor: int, word: string) =
        match number % factor with
        | 0 -> buffer + word
        | _ -> buffer
    let sentence: string = Array.fold appender "" dictionary
    match sentence with
    | "" -> number.ToString()
    | _ -> sentence
