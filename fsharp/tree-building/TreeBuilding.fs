module TreeBuilding

type Record = { RecordId: int; ParentId: int }
type Tree = 
    | Branch of int * Tree list
    | Leaf of int

let recordId t = 
    match t with
    | Branch (id, c) -> id
    | Leaf id -> id

let isBranch t = 
    match t with
    | Branch (id, c) -> true
    | Leaf id -> false

let children t = 
    match t with
    | Branch (id, c) -> c
    | Leaf id -> []

let buildIndex (previous:int, leaves:(int * int) list) (element:Record) =
    if (element.RecordId = 0 || (element.ParentId < element.RecordId)) then
        if element.RecordId = previous + 1 then
            (element.RecordId,
                match element.RecordId with
                | 0 -> (-1, element.RecordId) :: leaves
                | _ -> (element.ParentId, element.RecordId) :: leaves)
        else
            failwith "Non-continuous list"
    else
        failwith "Nodes with invalid parents"
    

let betterBuilder (records:Record list):Tree =
    let sortedRecords = List.sortBy (fun x -> x.RecordId) records
    match sortedRecords with
    | [] -> failwith "Empty input"
    | (root:Record)::(tail:Record list) ->
        match root.ParentId with
        | 0 ->
            match root.RecordId with
            | 0 ->
                sortedRecords
                |> List.fold buildIndex (-1, [])
            | _ -> failwith "Root node is invalid"
        | _ -> failwith "Root node is invalid"


let buildTree records = 
    let records' = List.sortBy (fun x -> x.RecordId) records

    if List.isEmpty records' then failwith "Empty input"
    else
        let root = records'.[0]
        if (root.ParentId = 0 |> not) then
            failwith "Root node is invalid"
        else
            if (root.RecordId = 0 |> not) then failwith "Root node is invalid"
            else
                let mutable prev = -1
                let mutable leafs = []

                for r in records' do
                    if (r.RecordId <> 0 && (r.ParentId > r.RecordId || r.ParentId = r.RecordId)) then
                        failwith "Nodes with invalid parents"
                    else
                        if r.RecordId <> prev + 1 then
                            failwith "Non-continuous list"
                        else                            
                            prev <- r.RecordId
                            if (r.RecordId = 0) then
                                leafs <- (-1, r.RecordId) :: leafs
                            else
                                leafs <- (r.ParentId, r.RecordId) :: leafs

                leafs <- List.rev leafs 
                let root = leafs.[0]

                let grouped = leafs |> List.groupBy fst |> List.map (fun (x, y) -> (x, List.map snd y))
                let parens = List.map fst grouped
                let map = grouped |> Map.ofSeq

                let rec helper key =
                    if Map.containsKey key map then
                        Branch (key, List.map (fun i -> helper i) (Map.find key map))
                    else
                        Leaf key                    

                let root = helper 0
                root