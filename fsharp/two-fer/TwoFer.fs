module TwoFer

open System

let twoFer (input:string option):string =
    let name:string = Option.defaultValue "you" input
    String.Format("One for {0}, one for me.", name)