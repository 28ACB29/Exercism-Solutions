module SimpleLinkedList

    //TODO:define LinkedList type
    type 'a LinkedList =
        | Nil
        | List of 'a * 'a LinkedList

    let private cons (head:'a) (tail:'a list):'a list = head::tail

    let rec private listBuilder (differenceList:'a list -> 'a list) (genericList:'a LinkedList):'a list =
        match genericList with
        | Nil -> differenceList []
        | List(head:'a, tail:'a LinkedList) -> listBuilder (differenceList << cons head) tail

    let rec private reverser (reversed:'a LinkedList) (genericList:'a LinkedList):'a LinkedList =
        match genericList with
        | Nil -> reversed
        | List(head:'a, tail:'a LinkedList) -> reverser (List(head, reversed)) tail

    let nil:'a LinkedList = Nil

    let create (x:'a) (n:'a LinkedList):'a LinkedList = List(x, n)

    let rec private linkedListBuilder (differenceList:'a LinkedList -> 'a LinkedList) (genericList:'a list):'a LinkedList =
        match genericList with
        | [] -> differenceList Nil
        | (head:'a)::(tail:'a list) -> linkedListBuilder (differenceList << create head) tail

    let isNil (x:'a LinkedList):bool =
        match x with
        | Nil -> true
        | _ -> false

    let next (x:'a LinkedList):'a LinkedList =
        match x with
        | Nil -> Nil
        | List(_, next:'a LinkedList) -> next

    let datum (x:'a LinkedList):'a =
        match x with
        | Nil -> failwith "Empty list"
        | List(datum:'a, _) -> datum

    let rec toList (x:'a LinkedList):'a list = listBuilder id x

    let rec fromList (xs:'a list):'a LinkedList = linkedListBuilder id xs

    let reverse (x:'a LinkedList):'a LinkedList = reverser Nil x