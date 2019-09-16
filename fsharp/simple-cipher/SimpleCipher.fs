module SimpleCipher

    open System

    let private alphabet:char array = [|'a'..'z'|]

    let private random ():int =
         let random:Random = Random()
         Async.Sleep(16)
         random.Next(26)

    let private randomLetter ():char = alphabet.[random()]

    let private generateKey () =
        Array.init 100 (fun _ -> randomLetter())
        |> String

    let private letterIndex (key:char) = Array.findIndex (fun (letter:char) -> letter = key) alphabet

    let private modulo (n: int) (m: int) =
        match Math.Sign(n) with
        | -1 -> (m - ((-n) % m))
        | 0 -> 0
        | 1 -> n % m

    let private moduloSum (a:int) (b:int) (m:int):int = modulo ((modulo a m) + (modulo b m)) m

    let private moduloDifference (a:int) (b:int) (m:int):int = modulo ((modulo a m) - (modulo b m)) m

    type SimpleCipher(key:string) =

        member __.Key with get():string = key

        member __.Encode(plaintext:string):string =
            plaintext.ToCharArray()
            |> Array.map2 (fun (key:char) (plain:char) -> alphabet.[moduloSum (letterIndex plain) (letterIndex key) 26]) (key.ToCharArray())
            |> String

        member __.Decode(ciphertext:string):string =
            ciphertext.ToCharArray()
            |> Array.map2 (fun (key:char) (cipher:char) -> alphabet.[moduloDifference (letterIndex cipher) (letterIndex key) 26]) (key.ToCharArray())
            |> String

        new() = SimpleCipher(generateKey())