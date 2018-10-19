module CustomSet

// TODO: define the Set type
type 'a Set = {elements:'a list}

let empty:'a Set = {elements = []}

let singleton (value:'a):'a Set = {elements = [value]}

let isEmpty (set:'a Set):bool = set.elements = []

let size (set:'a Set):int = List.length set.elements

let fromList (list:'a list):'a Set = {elements = List.distinct list}

let toList (set:'a Set):'a list = set.elements

let contains (value:'a) (set:'a Set):bool = List.contains value set.elements

let insert (value:'a) (set:'a Set):'a Set = {elements = List.distinct (value::set.elements)}

let union (left:'a Set) (right:'a Set):'a Set = {elements = List.distinct (left.elements @ right.elements)}

let intersection (left:'a Set) (right:'a Set):'a Set =
    match size left > size right with
    | true -> {elements = List.filter (fun (element:'a) -> contains element left) right.elements}
    | false -> {elements = List.filter (fun (element:'a) -> contains element right) left.elements}

let difference (left:'a Set) (right:'a Set):'a Set = {elements = List.filter (fun (element:'a) -> contains element right |> not) left.elements}

let isSubsetOf (left:'a Set) (right:'a Set):bool =
    left.elements
    |> List.forall (fun (element:'a) -> contains element right) 

let isDisjointFrom (left:'a Set) (right:'a Set):bool =
    intersection left right
    |> (=) empty

let isEqualTo (left:'a Set) (right:'a Set):bool =
    isSubsetOf left right && isSubsetOf right left