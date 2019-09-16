module HighScores

    open System

    let scores (values:int list):int list = values

    let latest (values:int list):int =
        values
        |> List.last

    let highest (values:int list):int =
        values
        |> List.max

    let top (values:int list):int list =
        let number:int = min (values.Length) 3
        values
        |> List.sortDescending
        |> List.take number

    let report (values:int list):string =
        let last:int = latest values
        let best:int = highest values
        if last = best then
            String.Format("Your latest score was {0}. That's your personal best!", best)
        else
            String.Format("Your latest score was {0}. That's {1} short of your personal best!", best, best - last)