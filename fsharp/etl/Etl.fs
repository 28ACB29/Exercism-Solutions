module Etl

    let transform (scoresWithLetters: Map<int, char list>): Map<char, int> =
        scoresWithLetters
        |> Map.toList
        |> List.collect (fun (score: int, letters: char list) -> letters |> List.map (fun (letter: char) -> (System.Char.ToLower(letter), score)))
        |> Map.ofList