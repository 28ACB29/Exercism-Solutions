module PascalsTriangle

    let private cons (head:'a) (tail:'a list) = head::tail

    let rec private scanner (differenceList:int list list -> int list list) (row:int list) (n:int):int list list =
        let newRow = List.map2(fun element1 element2 -> element1 + element2) (0::row) (0::row |> List.rev)
        match n with
        | 1 -> (differenceList << cons newRow) []
        | _ -> scanner (differenceList << cons newRow) newRow (n - 1)

    let rec private pascalsTriangle (k:int):int list list =
        match k with
        | 0 -> []
        | 1 -> [[1]]
        | _ -> scanner (id << cons [1]) ([1]) (k - 1)

    let rows (numberOfRows:int):int list list option =
        match numberOfRows > -1 with
        | true ->
            numberOfRows
            |> pascalsTriangle
            |> Some
        | false -> None