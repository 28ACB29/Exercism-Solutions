module RobotName

open System
open System.Threading

let private alphabet:char array = [|'A'..'Z'|]

let private digits:char array = [|'0'..'9'|]

type Robot = {name:string}

let private generator (maximum:int):int =
    Thread.Sleep(16)
    Random().Next(maximum)

let private generateLetter ():char = alphabet.[generator 26]

let private generateDigit ():char = digits.[generator 10]

let private createName ():string =
    [|generateLetter (); generateLetter (); generateDigit (); generateDigit (); generateDigit ()|]
    |> String

let mkRobot():Robot = {name = createName ()}

let name (robot:Robot):string = robot.name

let reset (robot:Robot):Robot = {robot with name = createName ()}