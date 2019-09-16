module Sieve

    let private cons (head:'a) (tail:'a list):'a list = head::tail

    let rec private sieve (differenceList:int list -> int list) (numbers:int list) =
        match numbers with
        | [] ->
            []
            |> differenceList
        | (head:int)::(tail:int list) ->
            tail
            |> List.filter (fun (element:int) -> element % head <> 0)
            |> sieve (differenceList << (cons head))

    let primes (limit:int):int list = sieve id [2..limit]