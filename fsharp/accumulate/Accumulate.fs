module Accumulate

    let private cons (head:'a) (tail:'a list) = head::tail

    let rec private builder (differenceList:'b list -> 'b list) (func: 'a -> 'b) (list:'a list):'b list =
        match list with
        | [] -> differenceList []
        | (head: 'a)::(tail: 'a list) -> builder (differenceList << cons (func head)) func tail

    let accumulate<'a, 'b> (func: 'a -> 'b) (input: 'a list): 'b list =
        input
        |> builder id func