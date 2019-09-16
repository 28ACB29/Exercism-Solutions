module BinarySearchTree

    type 'a binary_tree when 'a: comparison = Node of 'a * 'a binary_tree option * 'a binary_tree option

    let private cons (head:'a) (tail:'a list):'a list = head::tail

    let rec private insert (internalTree:'a binary_tree option) (element:'a when 'a: comparison):'a binary_tree option =
        match internalTree with
            | None -> (Some(Node(element, None, None)))
            | Some(Node(value:'a, left:'a binary_tree option, right:'a binary_tree option)) ->
                match element > value with
                | true -> Some(Node(value, left, insert right element))
                | false -> Some(Node(value, insert left element, right))

    let rec private inorder (internalTree:'a binary_tree option) (currentList:'a list when 'a: comparison):'a list when 'a: comparison =
        match internalTree with
        | None -> currentList
        | Some(Node(value:'a, left:'a binary_tree option, right:'a binary_tree option)) ->
            currentList
            |> inorder right
            |> cons value
            |> inorder left

    let left (node:'a binary_tree):'a binary_tree option =
        match node with
        | Node(_, left:'a binary_tree option, _) -> left

    let right (node:'a binary_tree):'a binary_tree option =
        match node with
        | Node(_, _, right:'a binary_tree option) -> right

    let data (node:'a binary_tree):'a =
        match node with
        | Node(value:'a, _, _) -> value

    let create (items:'a list when 'a: comparison):'a binary_tree =
        match items with
        | [] -> failwith "Cannot create tree from empty list."
        | _ ->
            items
            |> List.fold insert None
            |> Option.get

    let sortedData (node:'a binary_tree):'a list when 'a: comparison =
        match node with
        | Node(value:'a, left:'a binary_tree option, right:'a binary_tree option) ->
            []
            |> inorder right
            |> cons value
            |> inorder left