module RationalNumbers

open System

let rec private gcd (a:int) (b:int):int =
    match a, b with
    | a, b when a = b -> a
    | a, 0 -> a
    | 0, b -> b
    | a, b when a > b -> gcd (a % b) b
    | a, b when a < b -> gcd a (b % a)

type RationalNumber = {numerator:int; denominator:int}

let create (numerator:int) (denominator:int):RationalNumber = {numerator = numerator; denominator = denominator}

let reduce {numerator = a; denominator = b}:RationalNumber =
    match a, b with
    | 0, b -> create 0 1
    | a, 0 -> create 1 0
    | _ ->
        let absA:int = Math.Abs(a)
        let absB:int = Math.Abs(b)
        let common:int = gcd absA absB
        match Math.Sign(a) = Math.Sign(b) with
        | true -> {numerator = absA / common; denominator = absB / common}
        | false -> {numerator = -absA / common; denominator = absB / common}

let add ({numerator = a; denominator = b}:RationalNumber) ({numerator = c; denominator = d}:RationalNumber):RationalNumber =
    {numerator = a * d + b * c; denominator = b * d}
    |> reduce

let sub ({numerator = a; denominator = b}:RationalNumber) ({numerator = c; denominator = d}:RationalNumber):RationalNumber =
    {numerator = a * d - b * c; denominator = b * d}
    |> reduce

let mul ({numerator = a; denominator = b}:RationalNumber) ({numerator = c; denominator = d}:RationalNumber):RationalNumber =
    {numerator = a * c; denominator = b * d}
    |> reduce

let rec private fastPower (n:int) (r:RationalNumber) (accumulator:RationalNumber):RationalNumber =
    match n with
    | 0 -> accumulator
    | 1 -> mul r accumulator
    | _ ->
        match n % 2 with
        | 0 -> fastPower (n / 2) (mul r r) accumulator
        | 1 -> fastPower ((n - 1) / 2) (mul r r) (mul r accumulator)

let div ({numerator = a; denominator = b}:RationalNumber) ({numerator = c; denominator = d}:RationalNumber):RationalNumber =
    {numerator = a * d; denominator = b * c}
    |> reduce

let abs ({numerator = a; denominator = b}:RationalNumber):RationalNumber =
    {numerator = Math.Abs(a); denominator = Math.Abs(b)}
    |> reduce

let exprational (n:int) (r:RationalNumber):RationalNumber =
    fastPower n r (create 1 1)
    |> reduce

let expreal ({numerator = a; denominator = b}:RationalNumber) (x:int):double =
    Math.Pow(double x, (double a / double b))