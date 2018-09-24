module Sieve

let rec private sieve (numbers:int list) =
    match numbers with
    | [] -> []
    | (head:int)::(tail:int list) ->
        let filtered:int list =
            tail
            |> List.filter (fun (element:int) -> element % head <> 0)
        head::(sieve filtered)

let primes (limit:int):int list = sieve [2..limit]