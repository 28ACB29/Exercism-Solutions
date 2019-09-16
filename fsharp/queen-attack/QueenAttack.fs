module QueenAttack

    let private isOnBoard (location:int): bool = location > -1 && location < 8

    let private absoluteDifference (a:int) (b:int):int =
        a - b
        |> abs

    let private absoluteDistance (row1:int, column1:int) (row2:int, column2:int):int * int =
        let rowDistance:int = absoluteDifference row1 row2
        let columnDistance:int = absoluteDifference column1 column2
        (rowDistance, columnDistance)

    let create (row:int, column: int):bool =
        let validRow:bool =
            row
            |> isOnBoard
        let validColumn:bool =
            column
            |> isOnBoard
        validRow && validColumn

    let canAttack (queen1:int * int) (queen2:int * int):bool =
        let valid1:bool =
            queen1
            |> create
        let valid2:bool =
            queen2
            |> create
        let (rowDistance, columnDistance):int * int = absoluteDistance queen1 queen2
        valid1 && valid2 && (rowDistance = columnDistance || rowDistance = 0 || columnDistance = 0)