module SecretHandshake

    open System

    [<FlagsAttribute>]
    type private Move =
    | Wink = 1
    | DoubleBlink = 2
    | CloseYourEyes = 4
    | Jump = 8
    | Reverse = 16

    let private doMove (number:int) (move:Move):bool =
        move
        |> int
        |> (&&&) number
        |> (<>) 0

    let private (|IsReverse|_|) (number:int) =
        let reverseBit:int =
            Move.Reverse
            |> int
            |> (&&&) number
        match reverseBit with
        | 0 -> None
        | _ ->
            Move.Reverse
            |> int
            |> (-) number
            |> Some

    let private toString (move:Move):string =
        match move with
        | Move.Wink -> "wink"
        | Move.DoubleBlink -> "double blink"
        | Move.CloseYourEyes -> "close your eyes"
        | Move.Jump -> "jump"
        | Move.Reverse -> "reverse"

    let private sequence (number:int):string list =
        typeof<Move>
        |> Enum.GetValues
        |> Seq.cast<Move>
        |> Seq.filter (fun (move:Move) -> doMove number move)
        |> Seq.map toString
        |> List.ofSeq

    let commands (number:int):string list =
        match number with
        | IsReverse(newNumber:int) ->
            newNumber
            |> sequence
            |> List.rev
        | _ ->
            number
            |> sequence