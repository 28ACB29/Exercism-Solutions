module GameOfLife

let tick (input: int[,]):int[,] =
    let rows = input.GetLength(0)
    let cols = input.GetLength(1)
    let output = Array2D.create rows cols 0
    let getNeighbors (r: int) (c: int) =
        [ for dr in -1 .. 1 do
            for dc in -1 .. 1 do
                if not (dr = 0 && dc = 0) then
                    let nr, nc = r + dr, c + dc
                    if nr >= 0 && nr < rows && nc >= 0 && nc < cols then
                        yield input.[nr, nc] ]
    for r in 0 .. rows - 1 do
        for c in 0 .. cols - 1 do
            let liveNeighbors = getNeighbors r c |> List.sum
            match input.[r, c] with
            | 1 when liveNeighbors < 2 || liveNeighbors > 3 -> output.[r, c] <- 0 // Dies
            | 1 -> output.[r, c] <- 1 // Stays alive
            | 0 when liveNeighbors = 3 -> output.[r, c] <- 1 // Becomes alive
            | _ -> output.[r, c] <- input.[r, c] // Stays the same
    output