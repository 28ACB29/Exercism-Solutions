module Rectangles

    let private extractPoints (grid: char array array): (int * int) array =
        let height = Array.length grid
        let width = if height > 0 then Array.length grid.[0] else 0
        [|for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                if grid.[y].[x] = '+' then
                    yield (x, y)|]

    let private cornersExist (grid: char array array) (x1: int) (y1: int) (x2: int) (y2: int): bool =
        grid.[y1].[x1] = '+' && grid.[y1].[x2] = '+' && grid.[y2].[x1] = '+' && grid.[y2].[x2] = '+'

    let private horizontalEdge (grid: char array array) (y: int) (xStart: int) (xEnd: int): bool =
        match xEnd - xStart with
        | 1 -> true
        | _ ->
            seq{for x in xStart + 1 .. xEnd - 1 do
                yield grid.[y].[x] = '-'}
            |> Seq.forall id

    let private verticalEdge (grid: char array array) (x: int) (yStart: int) (yEnd: int): bool =
        match yEnd - yStart with
        | 1 -> true
        | _ ->
            seq{for y in yStart + 1 .. yEnd - 1 do
                yield grid.[y].[x] = '|'}
            |> Seq.forall id

    let private edgesExist (grid: char array array) (x1: int) (y1: int) (x2: int) (y2: int): bool =
        horizontalEdge grid y1 x1 x2 &&
        horizontalEdge grid y2 x1 x2 &&
        verticalEdge grid x1 y1 y2 &&
        verticalEdge grid x2 y1 y2

    let rectangles (lines: string list): int =
        let grid: char array array =
            lines
            |> List.map (fun (line:string) -> line.ToCharArray())
            |> List.toArray
        let points: (int * int) array = extractPoints grid
        seq{for point1: (int * int) in points do
             for point2: (int * int) in points do
                 if point1 <> point2 then
                    let (x1: int, y1: int) = point1
                    let (x2: int, y2: int) = point2
                    if x1 < x2 && y1 < y2 && cornersExist grid x1 y1 x2 y2 && edgesExist grid x1 y1 x2 y2 then
                        yield 1}
        |> Seq.sum