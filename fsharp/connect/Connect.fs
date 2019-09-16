module Connect

// TODO: implement this module
type Player =
    | White
    | Black

let winner (board:string list): Player option =