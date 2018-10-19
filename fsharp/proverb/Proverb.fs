﻿module Proverb

open System

let cons (head:'a) (tail:'a list) = head::tail

let createVerse (head1:string) (head2:string) = String.Format("For want of a {0} the {1} was lost.", head1, head2)

let rec createVerses (accumulator:string list) (rest:string list):string list =
    match rest with
    | (head1:string)::(head2:string)::(tail:string list) ->
        tail
        |> cons head2
        |> createVerses accumulator
        |> cons (createVerse head1 head2)
    | _ -> accumulator

let createEnd (start:string):string = String.Format("And all for the want of a {0}.", start)

let recite (input:string list):string list =
    input
    |> createVerses [createEnd input.Head]