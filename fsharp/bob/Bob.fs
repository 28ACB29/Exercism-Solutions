module Bob

let response (input:string):string =
    let trimmed:string = input.Trim()
    match trimmed with
    | trimmed when trimmed.EndsWith("?") -> "Sure."
    | trimmed when trimmed.EndsWith("!") -> "Whoa, chill out!"
    | trimmed when trimmed.EndsWith("?!") -> "Calm down, I know what I'm doing!"
    | "" -> "Fine. Be that way!"
    | _ -> "Whatever!"