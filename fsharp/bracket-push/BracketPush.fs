module BracketPush

let private brackets:char array = [|'(';')';'{';'}';'[';']';|]

let rec private FSM (stack:char list) (s:char list):bool =
    match stack, s with
    | [], [] -> true
    | stack, '('::sTail -> FSM ('('::stack) sTail
    | stack, '{'::sTail -> FSM ('{'::stack) sTail
    | stack, '['::sTail -> FSM ('['::stack) sTail
    | '('::stackTail, ')'::sTail -> FSM stackTail sTail
    | '{'::stackTail, '}'::sTail -> FSM stackTail sTail
    | '['::stackTail, ']'::sTail -> FSM stackTail sTail
    | _ -> false

let private isBalanced (S:string):bool =
    S.ToCharArray()
    |> Array.filter (fun (letter:char) -> Array.contains letter brackets)
    |> Array.toList
    |> FSM []

let isPaired (input:string):bool =
    isBalanced input