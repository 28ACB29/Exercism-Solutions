module Allergies

    open System

    // TODO: define the Allergen type
    [<FlagsAttribute>]
    type Allergen =
    | Eggs = 1
    | Peanuts = 2
    | Shellfish = 4
    | Strawberries = 8
    | Tomatoes = 16
    | Chocolate = 32
    | Pollen = 64
    | Cats = 128

    let allergicTo (codedAllergies:int) (allergen:Allergen):bool =
        allergen
        |> int
        |> (&&&) codedAllergies
        |> (<>) 0

    let list (codedAllergies:int):Allergen list =
        typeof<Allergen>
        |> Enum.GetValues
        |> Seq.cast<Allergen>
        |> Seq.filter (fun (allergen:Allergen) -> allergicTo codedAllergies allergen)
        |> List.ofSeq
