module DotDsl

type Attribute = {key:string; value:string}

type Edge = {left:string; right:string; attributes:Attribute list}

type Node = {key:string; attributes:Attribute list}

type Child =
    | Attribute of Attribute
    | Edge of Edge
    | Node of Node

type Graph = {children:Child list}

let toAttributes (attributes:(string * string) list):Attribute list =
    attributes
    |> List.map (fun (key:string, value:string) -> {key = key; value = value}:Attribute)

let selectAttribute (child:Child):Attribute option =
    match child with
    | Attribute(attribute:Attribute) -> attribute |> Some
    | _ -> None

let selectEdge (child:Child):Edge option =
    match child with
    | Edge(edge:Edge) -> edge |> Some
    | _ -> None

let selectNode (child:Child):Node option =
    match child with
    | Node(node:Node) -> node |> Some
    | _ -> None

let graph (children:Child list):Graph = {children = children}

let attr (key:string) (value:string):Child = Attribute{key = key; value = value}

let node (key:string) (attrs:(string * string) list):Child = Node{key = key; attributes = toAttributes attrs}

let edge (left:string) (right:string) (attrs:(string * string) list):Child = Edge{left = left; right = right; attributes = toAttributes attrs}

let attrs (graph:Graph):Child list =
    graph.children
    |> List.choose selectAttribute
    |> List.sortBy (fun {key = key} -> key)
    |> List.map (fun (attribute:Attribute) -> Attribute(attribute))

let nodes (graph:Graph):Child list =
    graph.children
    |> List.choose selectNode
    |> List.sortBy (fun {key = key} -> key)
    |> List.map (fun (node:Node) -> Node(node))

let edges (graph:Graph):Child list =
    graph.children
    |> List.choose selectEdge
    |> List.sortBy (fun {left = left} -> left)
    |> List.map (fun (edge:Edge) -> Edge(edge))