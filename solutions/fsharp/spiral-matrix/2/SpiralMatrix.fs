module SpiralMatrix

open System

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