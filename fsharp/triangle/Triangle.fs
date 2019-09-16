module Triangle

    let private (|IsTriangle|_|) (triangle:double list) =
        match triangle with
        | [a:double; b:double; c:double] when a + b > c && b + c > a && a + c > b ->
            [a; b; c]
            |> Some
        | _ -> None

    let equilateral (triangle:double list):bool =
        match triangle with
        | IsTriangle [a:double; b:double; c:double] -> a = b && b = c
        | _ -> false

    let isosceles (triangle:double list):bool =
        match triangle with
        | IsTriangle [a:double; b:double; c:double] -> a = b || b = c || c = a
        | _ -> false

    let scalene (triangle:double list):bool =
        match triangle with
        | IsTriangle [a:double; b:double; c:double] -> a <> b && b <> c && c <> a
        | _ -> false