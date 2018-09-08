module Matrix

let private createMatrix (matrix:string):int list list =
    matrix.Split [|'\n'|]
    |> Array.map(fun (row:string) -> row.Split [|' '|]|> Array.map(fun (element:string) -> element |> int) |> Array.toList)
    |> Array.toList


let row (index:int) (matrix:string):int list =
    matrix
    |> createMatrix
    |> List.item index

let column (index:int) (matrix:string):int list =
    matrix
    |> createMatrix
    |> List.map (fun (row:int list) -> List.item index row)
