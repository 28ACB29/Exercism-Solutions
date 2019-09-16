module Change

let rec makeChange (change: int list) (sortedCoins:int list) (target:int): int list option =
    match sortedCoins, target with
    | _, 0 ->
        change
        |> Some
    | (biggestCoin:int)::(_), target when biggestCoin <= target -> makeChange (biggestCoin::change) sortedCoins (target - biggestCoin)
    | (biggestCoin:int)::(smallerCoins:int list), target when biggestCoin > target -> makeChange change smallerCoins target
    | _ -> None

let findFewestCoins (coins:int list) (target:int): int list option =
    let sortedCoins:int list =
        coins
        |> List.sortDescending
    match target < sortedCoins.[sortedCoins.Length - 1] with
    | true -> None
    | false -> makeChange ([]) sortedCoins target