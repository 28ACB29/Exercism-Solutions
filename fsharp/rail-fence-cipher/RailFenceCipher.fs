module RailFenceCipher

    open System

    let private calculateRail (rails:int) (i:int):int =
        let lastIndex:int = rails - 1
        let section:int = i % (2 * lastIndex)
        lastIndex - Math.Abs(section - lastIndex)

    let private putOnRail (rails:int) (i:int) (genericArray:'a array):'a array =
        genericArray
        |> Array.indexed
        |> Array.filter (fun (j:int, _) -> i = calculateRail rails j)
        |> Array.map (fun (_, element:'a) -> element)

    let private fillRails (rails:int) (genericArray:'a array):'a array array = Array.init rails (fun (i:int) -> putOnRail rails i genericArray)

    let private chunkSize (rails:int) (length:int) (i:int):int =
        let lastIndex:int = rails - 1
        let complete:int = length / (2 * lastIndex)
        let section:int = i % (2 * lastIndex)
        match section with
        | 0 -> complete
        | section when section = 2 * lastIndex - 1 -> complete
        | _ -> 2 * complete + 1

    let private generateChunks (rails:int) (length:int):int array =
        Array.init rails (fun (i:int) -> chunkSize rails length i)

    let encode (rails:int) (message:string):string =
        message.ToCharArray()
        |> fillRails rails
        |> Array.map (fun (rail:char array) -> rail |> String)
        |> String.concat ""

    let decode (rails:int) (message:string):string =
        message.ToCharArray()
        |> fillRails rails
        |> Array.map (fun (rail:char array) -> rail |> String)
        |> String.concat ""