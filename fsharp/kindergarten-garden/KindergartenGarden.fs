module KindergartenGarden

// TODO: define the Plant type
type Plant =
    | Clover
    | Grass
    | Radishes
    | Violets

let private students:string array = [|"Alice"; "Bob"; "Charlie"; "David"; "Eve"; "Fred"; "Ginny"; "Harriet"; "Ileana"; "Joseph"; "Kincaid"; "Larry"|]

let private toPlant (letter:char): Plant option =
    match letter with
    | 'C' ->
        Plant.Clover
        |> Some
    | 'G' ->
        Plant.Grass
        |> Some
    | 'R' ->
        Plant.Radishes
        |> Some
    | 'V' ->
        Plant.Violets
        |> Some
    | _ -> None

let plants (diagram:string) (student:string): Plant list =
    let plants:char array array =
        diagram.Split [|'\n'|]
        |> Array.map (fun (row:string) -> row.ToCharArray())
    let index:int =
        students
        |> Array.findIndex (fun (element:string) -> element = student)
    [plants.[0].[2 * index]; plants.[0].[2 * index + 1]; plants.[1].[2 * index]; plants.[1].[2 * index + 1]]
    |> List.choose toPlant