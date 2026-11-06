module Matrix

    let private createRow (row:string) =
        row.Split [|' '|]
        |> Array.map int
        |> Array.toList

    let private createMatrix (matrix:string):int list list =
        matrix.Split [|'\n'|]
        |> Array.map createRow
        |> Array.toList

    let row (index:int) (matrix:string):int list =
        matrix
        |> createMatrix
        |> List.item index

    let column (index:int) (matrix:string):int list =
        matrix
        |> createMatrix
        |> List.map (List.item index)
