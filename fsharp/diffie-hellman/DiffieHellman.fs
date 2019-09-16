module DiffieHellman

open System
open System.Threading

let private generator (maximum:int):bigint =
    Thread.Sleep(16)
    Random().Next(maximum)
    |> bigint

let privateKey (primeP:bigint):bigint =
    match primeP > bigint(Int32.MaxValue) with
    | true ->
        let bigMax:bigint = bigint(Int32.MaxValue)
        let quotient:int = primeP / bigMax |> int
        let remainder:int = primeP % bigMax |> int
        generator quotient * bigMax + generator remainder
    | false ->
        generator (primeP |> int)

let publicKey (primeP:bigint) (primeG:bigint) (privateKey:bigint):bigint = bigint.ModPow(primeG, privateKey, primeP)

let secret (primeP:bigint) (publicKey:bigint) (privateKey:bigint):bigint = bigint.ModPow(publicKey, privateKey, primeP)