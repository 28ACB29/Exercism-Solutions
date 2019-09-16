module OcrNumbers

    open System

    let private ASCIIOffset:int = 48

    let private rowsForNumber:int = 4

    let private columnsForNumber:int = 3

    let private numbers:char array array array =
        [|
            [|[|' ';'_';' '|];
                [|'|';' ';'|'|];
                [|'|';'_';'|'|];
                [|' ';' ';' '|]|];
            [|[|' ';' ';' '|];
                [|' ';' ';'|'|];
                [|' ';' ';'|'|];
                [|' ';' ';' '|]|];
            [|[|' ';'_';' '|];
                [|' ';'_';'|'|];
                [|'|';'_';' '|];
                [|' ';' ';' '|]|];
            [|[|' ';'_';' '|];
                [|' ';'_';'|'|];
                [|' ';'_';'|'|];
                [|' ';' ';' '|]|];
            [|[|' ';' ';' '|];
                [|'|';'_';'|'|];
                [|' ';' ';'|'|];
                [|' ';' ';' '|]|];
            [|[|' ';'_';' '|];
                [|'|';'_';' '|];
                [|' ';'_';'|'|];
                [|' ';' ';' '|]|];
            [|[|' ';'_';' '|];
                [|'|';'_';' '|];
                [|'|';'_';'|'|];
                [|' ';' ';' '|]|];
            [|[|' ';'_';' '|];
                [|' ';' ';'|'|];
                [|' ';' ';'|'|];
                [|' ';' ';' '|]|];
            [|[|' ';'_';' '|];
                [|'|';'_';'|'|];
                [|'|';'_';'|'|];
                [|' ';' ';' '|]|];
            [|[|' ';'_';' '|];
                [|'|';'_';'|'|];
                [|' ';'_';'|'|];
                [|' ';' ';' '|]|]
        |]

    let private slice (matrix:'a array array) (top:int) (bottom:int) (left:int) (right:int):'a array array = [|for i in top..bottom -> [|for j in left..right-> matrix.[i].[j]|]|]
 
    let private columnChunk (chunkSize:int) (matrix:'a array array):'a array array array =
        let rows:int = matrix.Length
        let columns:int = matrix.[0].Length
        let lower (i:int):int = chunkSize * i
        let upper (i:int):int = min columns ((chunkSize * (i + 1)) - 1)
        Array.init (columns / chunkSize) (fun (i:int) -> slice matrix (0) (rows - 1) (lower i) (upper i))

    let private indexToCharacter (index:int option):char =
        match index with
        | None -> '?'
        | Some(number:int) ->
            number
            |> (+) ASCIIOffset
            |> char

    let private getCharacter (symbols:char array array):char =
        numbers
        |> Array.tryFindIndex (fun (element:char array array) -> element = symbols)
        |> indexToCharacter

    let private toDigits (rows:string array):string =
        rows
        |> Array.map (fun (row:string) -> row.ToCharArray())
        |> columnChunk columnsForNumber
        |> Array.map getCharacter
        |> String

    let convert (input:string list):string option =
        match input.Length % rowsForNumber, input.[0].Length % columnsForNumber with
        | 0, 0 ->
            input
            |> Array.ofList
            |> Array.chunkBySize rowsForNumber
            |> Array.map toDigits
            |> String.concat ","
            |> Some
        | _ -> None