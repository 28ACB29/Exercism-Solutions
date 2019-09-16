module LinkedList

    type 'a linkedList = {front:'a list; back:'a list}

    let mkLinkedList:'a linkedList = {front = []; back = []}

    let addToEmpty (newValue:'a) (linkedList:'a linkedList) = {front = [newValue]; back = []}

    let pop (linkedList:'a linkedList):'a * 'a linkedList =
        match linkedList with
        | {front = []; back = []} -> failwith "pop on empty."
        | {front = []; back = (head:'a)::(tail:'a list)} ->
            let (newHead:'a)::(newTail:'a list) = head::tail |> List.rev
            (newHead, {front = newTail; back = []})
        | {front = (head:'a)::(tail:'a list); back = _} -> (head, {linkedList with front = tail})

    let shift (linkedList:'a linkedList):'a * 'a linkedList =
        match linkedList with
        | {front = []; back = []} -> failwith "shift on empty."
        | {front = (head:'a)::(tail:'a list); back = []} ->
            let (newHead:'a)::(newTail:'a list) = head::tail |> List.rev
            (newHead, {front = []; back = newTail})
        | {front = _; back = (head:'a)::(tail:'a list)} -> (head, {linkedList with back = tail})

    let push (newValue:'a) (linkedList:'a linkedList):'a linkedList = {linkedList with front = newValue::linkedList.front}

    let unshift (newValue:'a) (linkedList:'a linkedList):'a linkedList = {linkedList with back = newValue::linkedList.back}