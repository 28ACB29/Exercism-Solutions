module SaddlePoints

    let private rowMaxima (matrix:'a list list):'a list =
        matrix
        |> List.map List.max

    let private columnMinima (matrix:'a list list):'a list =
        matrix
        |> List.reduce (fun (minima:'a list) (row:'a list) -> List.map2 (fun (minimum:'a) (element:'a) -> min minimum element) minima row)

    let saddlePoints (matrix:'a list list):(int * int) list =
        match matrix with
        | [[]] -> []
        | _ ->
            let maxima:'a list = rowMaxima matrix
            let minima:'a list = columnMinima matrix
            let findInRow (i:int) (row:'a list) =
                row
                |> List.mapi (fun (j:int) (element:'a) -> (i, j, element))
                |> List.filter (fun (i:int, j:int, element:'a) -> element = maxima.[i] && element = minima.[j])
                |> List.map (fun (i:int, j:int, element:'a) -> (i, j))
            matrix
            |> List.mapi findInRow
            |> List.collect id