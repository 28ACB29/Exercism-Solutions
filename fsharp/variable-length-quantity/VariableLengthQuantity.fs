module VariableLengthQuantity

// TODO: implement this module
let encodeUnsignedInt (number:uint32):byte list =
    let rec tailCall (stack:byte list) (remainder:uint32) =
        match remainder with
        | 0u -> stack
        | _ -> tailCall ((remainder &&& 0x00007fu |> byte)::stack) (remainder / 128u)
    match number with
    | 0u -> [0x0uy]
    | _ -> tailCall [] number

let encode (numbers:uint32 list):byte list =

let split (numbers:byte list):uint32 list list =

let decode (numbers:byte list):uint32 list option =