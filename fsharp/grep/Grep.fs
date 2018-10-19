module Grep

open System.IO
open System.Text

type Flags =
    | LineNumbers
    | NamesOnly
    | CaseInsensitive
    | Invert
    | EntireLines

let readFlag (flag:string):Flags =
    match flag with
    | "-n" -> LineNumbers
    | "-l" -> NamesOnly
    | "-i" -> CaseInsensitive
    | "-v" -> Invert
    | "-x" -> EntireLines
    | _ -> failwith "Invalid flag."

 
let convert (parsedFlags:Flags list) (pattern:string) (line:string):(string * string) =
    match List.contains Flags.CaseInsensitive parsedFlags with
    | true -> (pattern.ToLower(), line.ToLower())
    | false -> (pattern, line)

let doMatch (parsedFlags:Flags list) (realPattern:string, realLine:string) :bool =
    match List.contains Flags.EntireLines parsedFlags with
    | true -> realPattern = realLine
    | false -> realLine.Contains(realPattern)

let resultSwitch (parsedFlags:Flags list) (result:bool):bool =
    match List.contains Flags.Invert parsedFlags with
    | true -> not result
    | false -> result

let matchPattern (parsedFlags:Flags list) (pattern:string) (line:string):bool =
    convert parsedFlags pattern line
    |> doMatch parsedFlags
    |> resultSwitch parsedFlags

let matchLines (parsedFlags:Flags list) (pattern:string) (file:string) =
    File.ReadAllLines(file)
    |> Array.filter (matchPattern parsedFlags pattern)

let prependFileName (file:string option) (line:string):string =
    match file with
    | Some(fileName:string) -> fileName + line
    | None -> line

let prependLineNumber (parsedFlags:Flags list) (lineNumber:int) (line:string):string =
    match List.contains Flags.LineNumbers parsedFlags with
    | true -> lineNumber.ToString() + line
    | false -> line

let createMatchResult (parsedFlags:Flags list) (file:string option) (lineNumber:int) (line:string):string =
    line
    |> prependLineNumber parsedFlags lineNumber
    |> prependFileName file

let nameOrLines (parsedFlags:Flags list) (file:string) (multipleFiles:bool) (lines:string array):string array =
    match List.contains Flags.NamesOnly parsedFlags with
    | true ->
        match lines with
        | [||] -> [||]
        | _ -> [|file|]
    | false ->
        match multipleFiles with
        | true ->
            lines
            |> Array.mapi (fun (lineNumber:int) (line:string) -> createMatchResult parsedFlags (Some(file)) lineNumber line)
        | false ->
            lines
            |> Array.mapi (fun (lineNumber:int) (line:string) -> createMatchResult parsedFlags None lineNumber line)

let processFile (parsedFlags:Flags list) (pattern:string) (multipleFiles:bool) (file:string) =
    file
    |> matchLines parsedFlags pattern
    |> nameOrLines parsedFlags file multipleFiles
    |> List.ofArray

let grep (files:string list) (flagArguments:string list) (pattern:string):string list =
    let parsedFlags:Flags list =
        flagArguments
        |> List.map readFlag
    match files with
    | [] -> []
    | [file] ->
        file
        |> processFile parsedFlags pattern false
    | _ ->
        files
        |> List.collect (fun (file:string) -> file |> processFile parsedFlags pattern true)