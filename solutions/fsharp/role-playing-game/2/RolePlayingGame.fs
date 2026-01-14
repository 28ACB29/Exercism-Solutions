module RolePlayingGame

    let private (|LessThan|_|) (a:'a when 'a:comparison) (b:'a when 'a:comparison) =
        match b < a with
        | true ->
            ()
            |> Some
        | false -> None

    type Player = {
        Name: string option
        Level: int
        Health: int
        Mana: int option}

    let introduce (player: Player): string = 
        match player.Name with
        | None -> "Mighty Magician"
        | Some name -> name

    let revive (player: Player): Player option = 
        match player.Health with
        | 0 ->
            match player.Level with
            | LessThan 10 -> Some { player with Health = 100; Mana = None }
            | _ -> Some { player with Health = 100; Mana = Some (100) }
        | _ -> None

    let castSpell (manaCost: int) (player: Player): Player * int =
        match player.Mana with
        | Some mana ->
            match mana with
            | LessThan manaCost -> (player, 0)
            | _ -> ({ player with Mana = Some (mana - manaCost) }, 2 * manaCost)
        | None -> ({ player with Health = max 0 player.Health - manaCost }, 0)
