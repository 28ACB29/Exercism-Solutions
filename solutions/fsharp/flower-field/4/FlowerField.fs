module FlowerField

    let directions:(int * int) array = [| -1, -1; -1, 0; -1, 1; 0, -1; 0, 1; 1, -1; 1, 0; 1, 1 |]

    let private countAdjacentFlowers (garden:string array) (row:int) (col:int):int =
        directions
        |> Array.sumBy (fun (dr, dc) ->
            let newRow = row + dr
            let newCol = col + dc
            if newRow >= 0 && newRow < garden.Length &&
               newCol >= 0 && newCol < garden.[newRow].Length &&
               garden.[newRow].[newCol] = '*' then 1 else 0)

    let annotate (input:string list):string list =
        match input with
        | [] -> []
        | _ ->
            let garden = input |> List.toArray
            let height = garden.Length
            match height with
            | 0 -> [""]
            | _ ->
                let width = garden.[0].Length
                match width with
                | 0 -> [""]
                | _ ->

                    [ for row in 0 .. height - 1 do
                        for col in 0 .. width - 1 do
                            if garden.[row].[col] = '*' then
                                yield "*"
                            else
                                let count = countAdjacentFlowers garden row col
                                yield if count > 0 then count.ToString() else " " ]
                    |> List.chunkBySize width
                    |> List.map (String.concat "")