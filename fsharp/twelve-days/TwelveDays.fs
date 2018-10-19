module TwelveDays

open System

let toDay (day:int):string =
    match day with
    | 1 -> "first"
    | 2 -> "second"
    | 3 -> "third"
    | 4 -> "fourth"
    | 5 -> "fifth"
    | 6 -> "sixth"
    | 7 -> "seventh"
    | 8 -> "eighth"
    | 9 -> "ninth"
    | 10 -> "tenth"
    | 11 -> "eleventh"
    | 12 -> "twelfth"
    | _ -> ""

let createStart (day:int):string = String.Format("On the {0} day of Christmas my true love gave to me,", toDay day)

let toPresent (multiple:bool) (day:int):string =
    match day with
    | 1 ->
        match multiple with
        | true -> "and a Partridge in a Pear Tree."
        | false -> "a Partridge in a Pear Tree."
    | 2 -> "two Turtle Doves,"
    | 3 -> "three French Hens,"
    | 4 -> "four Calling Birds,"
    | 5 -> "five Gold Rings,"
    | 6 -> "six Geese-a-Laying,"
    | 7 -> "seven Swans-a-Swimming,"
    | 8 -> "eight Maids-a-Milking,"
    | 9 -> "nine Ladies Dancing,"
    | 10 -> "ten Lords-a-Leaping,"
    | 11 -> "eleven Pipers Piping,"
    | 12 -> "twelve Drummers Drumming,"
    | _ -> ""

let cons (head:'a) (tail:'a list) = head::tail

let createSong (day:int):string =
    List.init day (fun (i:int) -> toPresent (day > 1) (day - i))
    |> cons (createStart day)
    |> String.concat " "

let recite (start:int) (stop:int):string list =
    [start..stop]
    |> List.map createSong