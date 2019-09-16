module PrimeFactors

    let private cons (head:int) (tail:int list):int list = head::tail

    let rec private factorize (differenceList:int list -> int list) (number:int64) (primeFactor:int):int list =
        match number with
        | 1L -> differenceList []
        | _ ->
            match number % (int64 primeFactor) with
            | 0L -> factorize (differenceList << cons primeFactor) (number / (int64 primeFactor)) primeFactor
            | _ -> factorize differenceList number (primeFactor + 1)

    let factors (number:int64):int list =
        match number > 1L with
        | true ->
            factorize id number 2
        | false -> []