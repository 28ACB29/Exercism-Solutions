module SimpleCipher

    open System

    let private alphabet:char array = [|'a'..'z'|]

    // Use a single Random instance to avoid repeated seeding issues
    let private rng = Random()

    let private random ():int =
         rng.Next(26)

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
        | _ -> failwith "Not a valid sign"

    let private moduloSum (a:int) (b:int) (m:int):int = modulo ((modulo a m) + (modulo b m)) m

    let private moduloDifference (a:int) (b:int) (m:int):int = modulo ((modulo a m) - (modulo b m)) m

    type SimpleCipher(key:string) =

        member __.Key with get():string = key

        member __.Encode(plaintext:string):string =
            plaintext.ToCharArray()
            |> Array.map2 (fun (key:char) (plain:char) -> alphabet.[moduloSum (letterIndex key) (letterIndex plain) 26]) (key.ToCharArray())
            |> String

        member __.Decode(ciphertext:string):string =
            ciphertext.ToCharArray()
            |> Array.map2 (fun (key:char) (cipher:char) -> alphabet.[moduloDifference (letterIndex key) (letterIndex cipher) 26]) (key.ToCharArray())
            |> String

        new() = SimpleCipher(generateKey())