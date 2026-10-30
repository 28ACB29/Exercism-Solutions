module WordSearch

let makeRealGrid (grid:string list):char array array =
    grid
    |> List.map (fun (row:string) -> row.ToCharArray())
    |> Array.ofList

let extractDimensions (grid:char array array) (word:string):(int * int * int) =
    let rows:int = grid.Length
    let cols:int = if rows = 0 then 0 else grid.[0].Length
    let wordLength:int = word.Length
    (rows, cols, wordLength)

let searchLeftToRight (grid:char array array) (word:string):((int * int) * (int * int)) option =
    let (rows:int, cols:int, wordLength:int) = extractDimensions grid word
    let rec searchInRow (row:int) (col:int):((int * int) * (int * int)) option =
        match col + wordLength > cols with
        | true -> None
        | false ->
            let substring:string = System.String(grid.[row], col, wordLength)
            match substring  = word with
            | true -> Some ((col + 1, row + 1), (col + wordLength, row + 1))
            | false -> searchInRow row (col + 1)
    let rec searchInGrid (row:int):((int * int) * (int * int)) option =
        match row >= rows with
        | true -> None
        | false ->
            match searchInRow row 0 with
            | Some (pos:(int * int) * (int * int)) -> Some pos
            | None -> searchInGrid (row + 1)
    searchInGrid 0

let searchRightToLeft (grid:char array array) (word:string):((int * int) * (int * int)) option =
    let reversedWord:string = new string (word.ToCharArray() |> Array.rev)
    let (rows:int, cols:int, wordLength:int) = extractDimensions grid word
    let rec searchInRow (row:int) (col:int):((int * int) * (int * int)) option =
        match col - wordLength + 1 < 0 with
        | true -> None
        | false ->
            let substring:string = System.String(grid.[row], col - wordLength + 1, wordLength)
            match substring = reversedWord with
            | true -> Some ((col + 1, row + 1), (col - wordLength + 2, row + 1))
            | false -> searchInRow row (col - 1)
    let rec searchInGrid (row:int):((int * int) * (int * int)) option =
        match row >= rows with
        | true -> None
        | false ->
            match searchInRow row (cols - 1) with
            | Some (pos:(int * int) * (int * int)) -> Some pos
            | None -> searchInGrid (row + 1)
    searchInGrid 0

let searchTopToBottom (grid:char array array) (word:string):((int * int) * (int * int)) option =
    let (rows:int, cols:int, wordLength:int) = extractDimensions grid word
    let rec searchInColumn (col:int) (row:int):((int * int) * (int * int)) option =
        match row + wordLength > rows with
        | true -> None
        | false ->
            let substring:string = System.String(Array.init wordLength (fun i -> grid.[row + i].[col]))
            match substring = word with
            | true -> Some ((col + 1, row + 1), (col + 1, row + wordLength))
            | false -> searchInColumn col (row + 1)
    let rec searchInGrid (col:int):((int * int) * (int * int)) option =
        match col >= cols with
        | true -> None
        | false ->
            match searchInColumn col 0 with
            | Some (pos:(int * int) * (int * int)) -> Some pos
            | None -> searchInGrid (col + 1)
    searchInGrid 0

let searchBottomToTop (grid:char array array) (word:string):((int * int) * (int * int)) option =
    let rows:int = grid.Length
    let cols:int = if rows = 0 then 0 else grid.[0].Length
    let wordLength:int = word.Length
    let reversedWord:string = new string (word.ToCharArray() |> Array.rev)
    let rec searchInColumn (col:int) (row:int):((int * int) * (int * int)) option =
        match row - wordLength + 1 < 0 with
        | true -> None
        | false ->
            let substring:string = System.String(Array.init wordLength (fun i -> grid.[row - i].[col]))
            match substring = reversedWord with
            | true -> Some ((col + 1, row + 1), (col + 1, row - wordLength + 2))
            | false -> searchInColumn col (row - 1)
    let rec searchInGrid (col:int):((int * int) * (int * int)) option =
        match col >= cols with
        | true -> None
        | false ->
            match searchInColumn col (rows - 1) with
            | Some (pos:(int * int) * (int * int)) -> Some pos
            | None -> searchInGrid (col + 1)
    searchInGrid 0

let searchTopLeftToBottomRight (grid:char array array) (word:string):((int * int) * (int * int)) option =
    let (rows:int, cols:int, wordLength:int) = extractDimensions grid word
    let rec searchInDiagonal (row:int) (col:int):((int * int) * (int * int)) option =
        match row + wordLength > rows || col + wordLength > cols with
        | true -> None
        | false ->
            let substring:string = System.String(Array.init wordLength (fun i -> grid.[row + i].[col + i]))
            match substring = word with
            | true -> Some ((col + 1, row + 1), (col + wordLength, row + wordLength))
            | false -> searchInDiagonal row (col + 1)
    let rec searchInGrid (row:int):((int * int) * (int * int)) option =
        match row >= rows with
        | true -> None
        | false ->
            match searchInDiagonal row 0 with
            | Some (pos:(int * int) * (int * int)) -> Some pos
            | None -> searchInGrid (row + 1)
    searchInGrid 0

let searchBottomLeftToTopRight (grid:char array array) (word:string):((int * int) * (int * int)) option =
    let (rows:int, cols:int, wordLength:int) = extractDimensions grid word
    let reversedWord:string = new string (word.ToCharArray() |> Array.rev)
    let rec searchInDiagonal (row:int) (col:int):((int * int) * (int * int)) option =
        match row - wordLength + 1 < 0 || col + wordLength > cols with
        | true -> None
        | false ->
            let substring:string = System.String(Array.init wordLength (fun i -> grid.[row - i].[col + i]))
            match substring = reversedWord with
            | true -> Some ((col + 1, row + 1), (col + wordLength, row - wordLength + 2))
            | false -> searchInDiagonal row (col + 1)
    let rec searchInGrid (row:int):((int * int) * (int * int)) option =
        match row >= rows with
        | true -> None
        | false ->
            match searchInDiagonal row 0 with
            | Some (pos:(int * int) * (int * int)) -> Some pos
            | None -> searchInGrid (row + 1)
    searchInGrid 0

let searchTopRightToBottomLeft (grid:char array array) (word:string):((int * int) * (int * int)) option =
    let (rows:int, cols:int, wordLength:int) = extractDimensions grid word
    let reversedWord:string = new string (word.ToCharArray() |> Array.rev)
    let rec searchInDiagonal (row:int) (col:int):((int * int) * (int * int)) option =
        match row + wordLength > rows || col - wordLength + 1 < 0 with
        | true -> None
        | false ->
            let substring:string = System.String(Array.init wordLength (fun i -> grid.[row + i].[col - i]))
            match substring = reversedWord with
            | true -> Some ((col + 1, row + 1), (col - wordLength + 2, row + wordLength))
            | false -> searchInDiagonal row (col - 1)
    let rec searchInGrid (row:int):((int * int) * (int * int)) option =
        match row >= rows with
        | true -> None
        | false ->
            match searchInDiagonal row (cols - 1) with
            | Some (pos:(int * int) * (int * int)) -> Some pos
            | None -> searchInGrid (row + 1)
    searchInGrid 0

let searchBottomRightToTopLeft (grid:char array array) (word:string):((int * int) * (int * int)) option =
    let (rows:int, cols:int, wordLength:int) = extractDimensions grid word
    let rec searchInDiagonal (row:int) (col:int):((int * int) * (int * int)) option =
        match row - wordLength + 1 < 0 || col - wordLength + 1 < 0 with
        | true -> None
        | false ->
            let substring:string = System.String(Array.init wordLength (fun i -> grid.[row - i].[col - i]))
            match substring = word with
            | true -> Some ((col + 1, row + 1), (col - wordLength + 2, row - wordLength + 2))
            | false -> searchInDiagonal row (col - 1)
    let rec searchInGrid (row:int):((int * int) * (int * int)) option =
        match row >= rows with
        | true -> None
        | false ->
            match searchInDiagonal row (cols - 1) with
            | Some (pos:(int * int) * (int * int)) -> Some pos
            | None -> searchInGrid (row + 1)
    searchInGrid 0

let searchFunctions = [
    searchLeftToRight;
    searchRightToLeft;
    searchTopToBottom;
    searchBottomToTop;
    searchTopLeftToBottomRight;
    searchBottomLeftToTopRight;
    searchTopRightToBottomLeft;
    searchBottomRightToTopLeft
]

let search (grid:string list) (wordsToSearchFor:string list): Map<string, ((int * int) * (int * int)) option> =
    let charGrid:char array array =
        grid
        |> List.map (fun row -> row.ToCharArray())
        |> List.toArray
    let searchWord (word:string):((int * int) * (int * int)) option =
        let rec searchFunctionsRec funcs =
            match funcs with
            | [] -> None
            | f::fs ->
                match f charGrid word with
                | Some (pos:(int * int) * (int * int)) -> Some pos
                | None -> searchFunctionsRec fs
        searchFunctionsRec searchFunctions
    wordsToSearchFor
    |> List.map (fun word -> (word, searchWord word))
    |> Map.ofList