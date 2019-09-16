module Darts

    open System

    let private inner:double = 1.0

    let private middle:double = 5.0

    let private outer:double = 10.0

    let private radius (x:double) (y:double):double =
        (x * x + y * y)
        |> Math.Sqrt

    let private (|GreaterThan|_|) (a:'a) (b:'a) =
        if a < b then
            Some()
        else
            None

    let score (x:double) (y:double):int =
        let distance:double = radius x y
        match distance with
        | GreaterThan outer -> 0
        | GreaterThan middle -> 1
        | GreaterThan inner -> 5
        | _ -> 10