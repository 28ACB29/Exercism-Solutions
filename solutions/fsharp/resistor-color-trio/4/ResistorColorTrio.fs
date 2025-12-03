module ResistorColorTrio

    let private colorCode (color: string): int =
        match color with
        | "black" -> 0
        | "brown" -> 1
        | "red" -> 2
        | "orange" -> 3
        | "yellow" -> 4
        | "green" -> 5
        | "blue" -> 6
        | "violet" -> 7
        | "grey" -> 8
        | "white" -> 9
        | _  -> failwith "Not a vaild color"

    let private divideDown (value: int): int * int =
        let rec loop (v: int) (exponent: int): int * int =
            if v <> 0 && v % 10 = 0 then
                loop (v / 10) (exponent + 1)
            else
                (v, exponent)
        loop value 0

    let private putZeros (exponent: int): string =
        '0'
        |> Array.create exponent
        |> System.String

    let private suffix (exponent: int): string =
        match exponent with
        | v when v >= 9 -> (putZeros (v - 9)) + " gigaohms"
        | v when v >= 6 -> (putZeros (v - 6)) + " megaohms"
        | v when v >= 3 -> (putZeros (v - 3)) + " kiloohms"
        | _ -> (putZeros exponent) + " ohms"

    let label (colors: string list): string =
        match colors with
        | (color1: string)::(color2: string)::(color3: string)::_ ->
            let (value: int, exponent1: int) = divideDown ((colorCode color1) * 10 + (colorCode color2))
            let exponent2: int = colorCode color3
            (string value) + (suffix (exponent1 + exponent2))
        | _ -> ""
