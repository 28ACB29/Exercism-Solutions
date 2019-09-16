module BeerSong

    open System

    let private cons (head:'a) (tail:'a list) = head::tail

    let private countAndDrinkBottles (bottles:int):(string list -> string list) =
        match bottles with
        | 0 -> id << cons "No more bottles of beer on the wall, no more bottles of beer." << cons "Go to the store and buy some more, 99 bottles of beer on the wall."
        | 1 -> id << cons "1 bottle of beer on the wall, 1 bottle of beer." << cons "Take it down and pass it around, no more bottles of beer on the wall."
        | _ -> id << cons (String.Format("{0} bottles of beer on the wall, {0} bottles of beer.", bottles)) << cons (String.Format("Take one down and pass it around, {0} bottle of beer on the wall.", bottles - 1))

    let rec recite (startBottles:int) (takeDown:int):string list =
        match takeDown < 1 with
        | true -> []
        | false ->
            match startBottles < 0 with
            | true -> []
            | false -> (countAndDrinkBottles startBottles) (recite (startBottles - 1) (takeDown - 1))