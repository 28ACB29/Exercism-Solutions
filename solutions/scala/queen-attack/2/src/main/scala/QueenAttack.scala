case class Queen(val row:Int, val column: Int)

object Queen
{

    private def isOnBoard (location:Int):Boolean = location > -1 && location < 8

    def create(x:Int, y:Int):Option[Queen] =
    {
        def validRow:Boolean = isOnBoard(x)
        def validColumn:Boolean = isOnBoard(y)
        validRow && validColumn match
        {
            case true => Some(Queen(row = x, column = y))
            case false => None
        }
    }
}

object QueenAttack
{

    private def absoluteDifference (a:Int, b:Int):Int = Math.abs(a - b)

    private def absoluteDistance (queen1:Queen, queen2:Queen):(Int, Int) =
    {
        def rowDistance:Int = absoluteDifference(queen1.row, queen2.row)
        def columnDistance:Int = absoluteDifference(queen1.column, queen2.column)
        (rowDistance, columnDistance)
    }

    def canAttack (queen1:Queen, queen2:Queen):Boolean =
    {
        def distance:(Int, Int) = absoluteDistance(queen1, queen2)
        def rowDistance:Int = distance._1
        def columnDistance:Int = distance._2
        rowDistance == columnDistance || rowDistance == 0 || columnDistance == 0
    }

}