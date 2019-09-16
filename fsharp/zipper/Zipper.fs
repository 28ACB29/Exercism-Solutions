module Zipper

// TODO: implement this module

type 'a Tree = Node of 'a * 'a Tree option * 'a Tree option

type Direction =
    | Left
    | Right

type 'a Zipper = {stack:('a Tree * Direction) list; current:'a Tree}

let tree (value:'a) (left:'a Tree option) (right:'a Tree option):'a Tree = Node(value, left, right)

let fromTree (tree:'a Tree):'a Zipper = {stack = []; current = tree}

let toTree (zipper:'a Zipper):'a Tree = zipper.current

let value (zipper:'a Zipper):'a =
    match zipper.current with
    | Node(value:'a, left:'a Tree option, right:'a Tree option) -> value

let left (zipper:'a Zipper):'a Zipper option =
    match zipper.current with
    | Node(value:'a, left:'a Tree option, right:'a Tree option) ->
        match left with
        | None -> None
        | Some(child:'a Tree) -> {stack = (zipper.current, Left)::zipper.stack; current = child} |> Some

let right (zipper:'a Zipper):'a Zipper option =
    match zipper.current with
    | Node(value:'a, left:'a Tree option, right:'a Tree option) ->
        match right with
        | None -> None
        | Some(child:'a Tree) -> {stack = (zipper.current, Right)::zipper.stack; current = child} |> Some

let up (zipper:'a Zipper):'a Zipper option =
    match zipper.stack with
    | [] -> None
    | (tree:'a Tree, direction:Direction)::(tail:('a Tree * Direction) list) -> {stack = tail; current = tree} |> Some

let setValue (newValue:'a) (zipper:'a Zipper):'a Zipper =
    match zipper.current with
    | Node(value:'a, left:'a Tree option, right:'a Tree option) -> {stack = zipper.stack; current = Node(newValue, left, right)}

let setLeft (newTree:'a Tree option) (zipper:'a Zipper):'a Zipper =
    match zipper.current with
    | Node(value:'a, left:'a Tree option, right:'a Tree option) -> {stack = zipper.stack; current = Node(value, newTree, right)}

let setRight (newTree:'a Tree option) (zipper:'a Zipper):'a Zipper =
    match zipper.current with
    | Node(value:'a, left:'a Tree option, right:'a Tree option) -> {stack = zipper.stack; current = Node(value, left, newTree)}