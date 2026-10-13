module SpiralMatrix

open System

let (|EqualTo|_|) (a:'a when 'a:comparison) (b:'a when 'a:comparison) =
    match b = a with
    | true ->
        ()
        |> Some
    | false -> None

// Multi-case active pattern that classifies a cell's side within its ring
let (|Top|Right|Bottom|Left|Center|) (r:int, c:int, k:int, n:int) =
    let m:int = n - 2 * k
    let edge:int = n - k - 1
    match m with
    // center
    | 1 -> Center
    | _ ->
        match (r, c) with
        // top
        | EqualTo k, _ -> Top
        // right
        | _, EqualTo edge -> Right
        // bottom
        | EqualTo edge, _ -> Bottom
        // left
        | _ -> Left

let spiralMatrix (size:int): int list list =
    if size = 0 then [] else
    let n:int = size
    let mid:int = n - 1
    let valueAt (r:int) (c:int) : int =
        // compute the ring using distance from the center
        // use integer abs on doubled coordinates to compute layer k
        let maxAbs2:int = max (Math.Abs(2 * r - mid)) (Math.Abs(2 * c - mid))
        let k:int = (mid - maxAbs2) / 2
        // side length of this ring
        let m:int = n - 2 * k
        // first value in this ring
        let start:int = n * n - m * m + 1
        if m = 1 then start
        else
            // top
            if r = k then start + (c - k)
            // right
            elif c = n - k - 1 then start + (m - 1) + (r - k)
            // bottom
            elif r = n - k - 1 then start + 2 * (m - 1) + (n - k - 1 - c)
            // left
            else start + 3 * (m - 1) + (n - k - 1 - r)

    [ for r in 0..n - 1 -> [ for c in 0..n - 1 -> valueAt r c ] ]