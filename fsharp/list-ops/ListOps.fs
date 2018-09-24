module ListOps

let rec private counter (accumulator:int) (list:'a list):int =
    match list with
    | [] -> accumulator
    | (head:'a)::(tail:'a list) -> counter (accumulator + 1) tail

let rec private reverser (reversed:'a list) (list:'a list):'a list =
    match list with
    | [] -> reversed
    | (head:'a)::(tail:'a list) -> reverser (head::reversed) tail

let rec private mapper (f:'a -> 'b) (list:'a list):'b list =
    match list with
    | [] -> []
    | (head:'a)::(tail:'a list) -> (f head)::(mapper f tail)

let rec private sieve (f:'a -> bool) (list:'a list):'a list =
    match list with
    | [] -> []
    | (head:'a)::(tail:'a list) ->
        match f head with
        | true -> head::(sieve f tail)
        | false -> sieve f tail

let rec private appender (xs:'a list) (ys:'a list):'a list =
    match xs with
    | [] -> ys
    | (head:'a)::(tail:'a list) -> head::(appender tail ys)

let rec foldl (folder:'b -> 'a -> 'b) (state:'b) (list:'a list):'b =
    match list with
    | [] -> state
    | (head:'a)::(tail:'a list) -> foldl folder (folder state head) tail

let rec foldr (folder:'a -> 'b -> 'b) (state:'b) (list:'a list):'b =
    match list with
    | [] -> state
    | (head:'a)::(tail:'a list) -> folder head (foldr folder state tail)

let length (list:'a list):int = counter 0 list

let reverse (list:'a list):'a list = reverser [] list

let map (f:'a -> 'b) (list:'a list) = mapper f list

let filter (f:'a -> bool) (list:'a list):'a list = sieve f list

let append (xs:'a list) (ys:'a list):'a list = appender xs ys

let rec private chainer (chainedList:'a list) (nestedList:'a list list):'a list =
    match nestedList with
    | [] -> chainedList
    | (head:'a list)::(tail:'a list list) -> append head (chainer chainedList tail)

let concat (xs:'a list list):'a list = chainer [] xs