module RomanNumerals

open System

let private cons (head:'a) (tail:'a list) = head::tail

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
    | arabicNumeral when arabicNumeral >= 1000 -> convert (differenceList << M) (arabicNumeral - 1000)
    | arabicNumeral when arabicNumeral >= 900 -> convert (differenceList << CM) (arabicNumeral - 900)
    | arabicNumeral when arabicNumeral >= 500 -> convert (differenceList << D) (arabicNumeral - 500)
    | arabicNumeral when arabicNumeral >= 400 -> convert (differenceList << CD) (arabicNumeral - 400)
    | arabicNumeral when arabicNumeral >= 100 -> convert (differenceList << C) (arabicNumeral - 100)
    | arabicNumeral when arabicNumeral >= 90 -> convert (differenceList << XC) (arabicNumeral - 90)
    | arabicNumeral when arabicNumeral >= 50 -> convert (differenceList << L) (arabicNumeral - 50)
    | arabicNumeral when arabicNumeral >= 40 -> convert (differenceList << XL) (arabicNumeral - 40)
    | arabicNumeral when arabicNumeral >= 10 -> convert (differenceList << X) (arabicNumeral - 10)
    | arabicNumeral when arabicNumeral >= 9 -> convert (differenceList << IX) (arabicNumeral - 9)
    | arabicNumeral when arabicNumeral >= 5 -> convert (differenceList << V) (arabicNumeral - 5)
    | arabicNumeral when arabicNumeral >= 4 -> convert (differenceList << IV) (arabicNumeral - 4)
    | arabicNumeral when arabicNumeral >= 1 -> convert (differenceList << I) (arabicNumeral - 1)
    | _ -> differenceList []

let roman (arabicNumeral:int):string =
    arabicNumeral
    |> convert id
    |> Array.ofList
    |> String
