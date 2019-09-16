module CircularBuffer

type CircularBuffer = {array:int option array; position:int}

let private advance (size:int) (position:int):int =
    match size with
    | 1 -> 0
    | _ -> (position + 1) % size

let mkCircularBuffer (size:int):CircularBuffer = {array = Array.replicate size None; position = 0}

let clear (buffer:CircularBuffer):CircularBuffer = {array = [||]; position = 0}

let write (value:int) (buffer:CircularBuffer):CircularBuffer =
    let newPosition:int = advance buffer.array.Length buffer.position
    match buffer.array.[newPosition] with
    | Some(_) -> failwith "Cannot write."
    | None ->
        let newBuffer:CircularBuffer = {array = buffer.array; position = newPosition}
        newBuffer.array.[newPosition] <- Some(value)
        newBuffer

let forceWrite (value:int) (buffer:CircularBuffer):CircularBuffer =
    let newPosition:int = advance buffer.array.Length buffer.position
    let newBuffer:CircularBuffer = {array = buffer.array; position = newPosition}
    newBuffer.array.[newPosition] <- Some(value)
    newBuffer

let read (buffer:CircularBuffer):(int * CircularBuffer) =
    let newPosition:int = advance buffer.array.Length buffer.position
    match buffer.array.[newPosition] with
    | Some(value:int) -> (value, buffer)
    | None -> failwith "Cannot read."