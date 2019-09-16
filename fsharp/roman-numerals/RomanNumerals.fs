module RomanNumerals

    open System

    let private cons (head:'a) (tail:'a list) = head::tail

    let (|GreaterThanOrEqualTo|_|) (a:'a when 'a:comparison) (b:'a when 'a:comparison) =
        match b >= a with
        | true ->
            ()
            |> Some
        | false -> None

    let private M:(char list -> char list) = cons 'M'

    let private CM:(char list -> char list) = cons 'C' << cons 'M'

    let private D:(char list -> char list) = cons 'D'

    let private CD:(char list -> char list) = cons 'C' << cons 'D'

    let private C:(char list -> char list) = cons 'C'

    let private XC:(char list -> char list) = cons 'X' << cons 'C'

    let private L:(char list -> char list) = cons 'L'

    let private XL:(char list -> char list) = cons 'X' << cons 'L'

    let private X:(char list -> char list) = cons 'X'

    let private IX:(char list -> char list) = cons 'I' << cons 'X'

    let private V:(char list -> char list) = cons 'V'

    let private IV:(char list -> char list) = cons 'I' << cons 'V'

    let private I:(char list -> char list) = cons 'I'

    let rec private convert (differenceList:(char list -> char list)) (arabicNumeral:int):char list =
        match arabicNumeral with
        | GreaterThanOrEqualTo 1000 -> convert (differenceList << M) (arabicNumeral - 1000)
        | GreaterThanOrEqualTo 900 -> convert (differenceList << CM) (arabicNumeral - 900)
        | GreaterThanOrEqualTo 500 -> convert (differenceList << D) (arabicNumeral - 500)
        | GreaterThanOrEqualTo 400 -> convert (differenceList << CD) (arabicNumeral - 400)
        | GreaterThanOrEqualTo 100 -> convert (differenceList << C) (arabicNumeral - 100)
        | GreaterThanOrEqualTo 90 -> convert (differenceList << XC) (arabicNumeral - 90)
        | GreaterThanOrEqualTo 50 -> convert (differenceList << L) (arabicNumeral - 50)
        | GreaterThanOrEqualTo 40 -> convert (differenceList << XL) (arabicNumeral - 40)
        | GreaterThanOrEqualTo 10 -> convert (differenceList << X) (arabicNumeral - 10)
        | GreaterThanOrEqualTo 9 -> convert (differenceList << IX) (arabicNumeral - 9)
        | GreaterThanOrEqualTo 5 -> convert (differenceList << V) (arabicNumeral - 5)
        | GreaterThanOrEqualTo 4 -> convert (differenceList << IV) (arabicNumeral - 4)
        | GreaterThanOrEqualTo 1 -> convert (differenceList << I) (arabicNumeral - 1)
        | _ -> differenceList []

    let roman (arabicNumeral:int):string =
        arabicNumeral
        |> convert id
        |> Array.ofList
        |> String
