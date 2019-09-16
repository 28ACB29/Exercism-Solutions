module WordSearch

let makeRealGrid (grid:string list):char array array =
    grid
    |> List.map (fun (row:string) -> row.ToCharArray())
    |> Array.ofList

let search (grid:string list) (wordsToSearchFor:string list): Map<string, ((int * int) * (int * int)) option> = failwith "You need to implement this function."