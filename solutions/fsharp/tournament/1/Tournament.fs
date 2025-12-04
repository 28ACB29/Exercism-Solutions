module Tournament

type outcome =
    | Win = 3
    | Draw = 1
    | Loss = 0

let convertLine (line:string):(string * outcome) list =
    let parts:string array = line.Split [|';'|]
    match parts.[2] with
    | "win" -> [(parts.[0], outcome.Win); (parts.[1], outcome.Loss)]
    | "draw" -> [(parts.[0], outcome.Draw); (parts.[1], outcome.Draw)]
    | "loss" -> [(parts.[0], outcome.Loss); (parts.[1], outcome.Win)]

let update (count:int, wins:int, draws:int, losses:int, points:int) (_, result:outcome):(int * int * int * int * int) =
    match result with
    | outcome.Win -> (count + 1, wins + 1, draws, losses, points + int result)
    | outcome.Draw -> (count + 1, wins, draws + 1, losses, points + int result)
    | outcome.Loss -> (count + 1, wins, draws, losses + 1, points + int result)
    | _ -> (count, wins, draws, losses, points)

let summarize (team:string, results:(string * outcome) list) =
    (team, results |> List.fold update (0, 0, 0, 0, 0))

let createRow (team:string, (mp:int, w:int, d:int, l:int, p:int)) : string =
    sprintf "%-31s| %2d | %2d | %2d | %2d | %2d" team mp w d l p

let createTable (summaries:(string * (int * int * int * int * int)) list):string list =
    let header = "Team                           | MP |  W |  D |  L |  P"
    let rows = 
        summaries
        |> List.sortBy (fun (_, (_, _, _, _, p)) -> -p)
        |> List.map createRow
    header :: rows

let tally (input:string list):string list =
    input
    |> List.collect convertLine
    |> List.groupBy (fun (team:string, _) -> team)
    |> List.map summarize
    |> createTable