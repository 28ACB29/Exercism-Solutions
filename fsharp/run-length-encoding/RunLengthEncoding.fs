module RunLengthEncoding

open System.Text

let private encodeBack (element:'a) (accumulator:(int * 'a) list):(int * 'a) list =
    match element, accumulator with
    | element, (count:int, item:'a)::(next:(int * 'a) list) when element = item -> (count + 1, element)::next
    | _ -> (1, element)::accumulator

let private realEncode (data:'a array):(int * 'a) list =
    Array.foldBack encodeBack data []

let encode (input:string):string =
    input.ToCharArray()
    |> realEncode
    |> List.fold (fun (buffer:StringBuilder) (count: int, item: char) -> if count = 1 then buffer.Append(item) else buffer.Append(count).Append(item)) (StringBuilder())
    |> (fun (buffer:StringBuilder) -> buffer.ToString())

let private realDecode (encoded:(int * 'a) list):'a list =
    encoded
    |> List.collect (fun (count: int, item: 'a) -> List.init count (fun _ -> item))

let decode (input:string):string = failwith "You need to implement this function."