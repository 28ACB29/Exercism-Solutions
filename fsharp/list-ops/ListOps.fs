module ListOps

    let private cons (head:'a) (tail:'a list) = head::tail

    let rec private differenceListCreator (differenceList:'a list -> 'a list) (list:'a list):'a list -> 'a list =
        match list with
        | [] -> differenceList
        | (head:'a)::(tail:'a list) -> differenceListCreator (differenceList << cons head) tail

    let rec private counter (accumulator:int) (list:'a list):int =
        match list with
        | [] -> accumulator
        | (head:'a)::(tail:'a list) -> counter (accumulator + 1) tail

    let rec private reverser (reversed:'a list) (list:'a list):'a list =
        match list with
        | [] -> reversed
        | (head:'a)::(tail:'a list) -> reverser (head::reversed) tail

    let rec private mapper (differenceList:'b list -> 'b list) (f:'a -> 'b) (list:'a list):'b list =
        match list with
        | [] -> differenceList []
        | (head:'a)::(tail:'a list) -> mapper (differenceList << cons (f head)) f tail

    let rec private sieve (differenceList:'a list -> 'a list) (f:'a -> bool) (list:'a list):'a list =
        match list with
        | [] -> differenceList []
        | (head:'a)::(tail:'a list) ->
            match f head with
            | true -> sieve (differenceList << cons head) f tail
            | false -> sieve differenceList f tail

    let rec private appender (differenceList:'a list -> 'a list) (xs:'a list) (ys:'a list):'a list =
        match xs with
        | [] ->
            []
            |> differenceListCreator differenceList ys
        | (head:'a)::(tail:'a list) -> appender (differenceList << cons head) tail ys

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

    let map (f:'a -> 'b) (list:'a list) = mapper id f list

    let filter (f:'a -> bool) (list:'a list):'a list = sieve id f list

    let append (xs:'a list) (ys:'a list):'a list = appender id xs ys

    let concat (xs:'a list list):'a list =  foldr append [] xs