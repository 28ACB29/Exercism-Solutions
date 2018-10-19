module GradeSchool

type School = Map<int, string list>

let cons (head:'a) (tail:'a list) = head::tail

let empty:School =
    [||]
    |> Map.ofArray

let add (student:string) (grade:int) (school:School):School =
    match school.TryFind grade with
    | None -> school.Add (grade, student::[])
    | Some(students: string list) -> school.Add (grade, students |> cons student |> List.sort)

let roster (school:School):(int * string list) list =
    school
    |> Map.toList

let grade (number:int) (school:School):string list =
    match school.TryFind number with
    | None -> []
    | Some(students: string list) -> students
