module SimpleLinkedList

//TODO: define LinkedList type
type 'a LinkedList =
    | Nil
    | List of 'a * 'a LinkedList

let rec tailCall (reversed: 'a LinkedList) (genericList: 'a LinkedList): 'a LinkedList =
    match genericList with
    | Nil -> reversed
    | List(head: 'a, tail: 'a LinkedList) -> tailCall (List(head, reversed)) tail

let nil:'a LinkedList = Nil

let create (x:'a) (n:'a LinkedList):'a LinkedList = List(x, n)

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

let rec toList (x:'a LinkedList):'a list =
    match x with
    | Nil -> []
    | List(datum:'a, next:'a LinkedList) -> datum::(toList next)

let rec fromList (xs:'a list):'a LinkedList =
    match xs with
    | [] -> Nil
    | (head:'a)::(tail:'a list) -> List(head, fromList tail)

let reverse (x:'a LinkedList):'a LinkedList = tailCall Nil x