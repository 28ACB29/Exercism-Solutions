module Accumulate

let accumulate<'a, 'b> (func: 'a -> 'b) (input: 'a list): 'b list =
    let rec tailCall (stack: 'b list) (remainder: 'a list) =
        match remainder with
        | [] -> stack
        | (head: 'a)::(tail: 'a list) -> tailCall ((func head)::stack) tail
    let rec reverse (forward: 'b list) (reversed: 'b list) =
        match reversed with
        | [] -> forward
        | (head: 'b)::(tail: 'b list) -> reverse (head::forward) tail
    input
    |> tailCall []
    |> reverse []