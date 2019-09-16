module GradeSchool

    type Student = string

    type Grade = int

    type School = Map<int, Student list>

    let private cons (head:'a) (tail:'a list) = head::tail

    let empty:School = Map.empty

    let add (student:Student) (grade:Grade) (school:School):School =
        school
        |> Map.tryFind grade
        |> Option.defaultValue []
        |> (cons student >> List.sort)
        |> (fun (students: Student list) -> Map.add grade students school)

    let roster (school:School):(Grade * Student list) list =
        school
        |> Map.toList

    let grade (number:Grade) (school:School):Student list =
        school
        |> Map.tryFind number
        |> Option.defaultValue []
