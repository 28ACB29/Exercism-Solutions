module Tournament

type outcome =
    | Win = 3
    | Draw = 1
    | Loss = 0

let convertLine (line:string):(string * outcome) list =
    let parts:string array = line.Split [|';'|]
    match parts with
    | [|(home:string); (away:string); (result:string)|] -> 
        match result with
        | "win" -> [(home, outcome.Win); (away, outcome.Loss)]
        | "draw" -> [(home, outcome.Draw); (away, outcome.Draw)]
        | "loss" -> [(home, outcome.Loss); (away, outcome.Win)]
    | _ -> []

let update (count:int, wins:int, draws:int, losses:int, points:int) (_, result:outcome):(int * int * int * int * int) =
    match result with
    | outcome.Win -> (count + 1, wins + 1, draws, losses, points + int result)
    | outcome.Draw -> (count + 1, wins, draws + 1, losses, points + int result)
    | outcome.Loss -> (count + 1, wins, draws, losses + 1, points + int result)
    | _ -> (count, wins, draws, losses, points)

let summarize (team:string, results:(string * outcome) list) =
    (team, results |> List.fold update (0, 0, 0, 0, 0))

let createRow (team:string, (mp:int, wins:int, draws:int, losses:int, points:int)) : string =
    sprintf "%-31s| %2d | %2d | %2d | %2d | %2d" team mp wins draws losses points

let createTable (summaries:(string * (int * int * int * int * int)) list):string list =
    let header:string = "Team                           | MP |  W |  D |  L |  P"
    let rows:string list = 
        summaries
        |> List.sortBy (fun (team:string, (_, _, _, _, points:int)) -> (-points, team))
        |> List.map createRow
    header :: rows

let tally (input:string list):string list =
    input
    |> List.collect convertLine
    |> List.groupBy (fun (team:string, _) -> team)
    |> List.map summarize
    |> createTable