module BeerSong

let countAndDrinkBottles (bottles:int):string list =
    match bottles with
    | 0 -> ["No more bottles of beer on the wall, no more bottles of beer."; "Go to the store and buy some more, 99 bottles of beer on the wall."]
    | 1 -> ["1 bottle of beer on the wall, 1 bottle of beer."; "Take it down and pass it around, no more bottles of beer on the wall."]
    | _ -> [bottles.ToString() + " bottles of beer on the wall, " + bottles.ToString() + " bottles of beer."; "Take one down and pass it around, " + (bottles - 1).ToString() + " bottle of beer on the wall."]

let rec recite (startBottles:int) (takeDown:int):string list =
    match takeDown < 1 with
    | true -> []
    | false ->
        match startBottles < 0 with
        | true -> []
        | false -> (countAndDrinkBottles startBottles) @ (recite (startBottles - 1) (takeDown - 1))