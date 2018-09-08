module TwoFer

open System

let twoFer (input:string option):string =
    let name:string =
        match input with
        | None -> "you"
        | Some(realName: string) -> realName
    String.Format("One for {0}, one for me.", name)