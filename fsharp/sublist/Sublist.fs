module Sublist

    type SublistType =
        | Equal
        | Sublist
        | Superlist
        | Unequal

    let sublist (xs:'a list) (ys:'a list):SublistType =
        match List.except ys xs, List.except xs ys with
        | [], [] -> Equal
        | [], _ -> Sublist
        | _, [] -> Superlist
        | _, _ -> Unequal