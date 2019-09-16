module SgfParsing

// TODO: implement this module
type node = SGFNode of Map<string, string list> * node list

let Node (dictionary:Map<string, string list>, children:node list):node = SGFNode(dictionary, children)

let parse (sgf:string):option Node =
    