object PascalsTriangle
{

    private def scanner (stack:List[List[Int]]) (row:List[Int]) (n:Int):List[List[Int]] =
    {
        def extended:List[Int] = 0::row
        def extendedReverse:List[Int] = extended.reverse
        def newRow:List[Int] = extended.zip(extendedReverse).map(elements => elements._1 + elements._2)
        n match
        {
            case 1 => (newRow::stack).reverse
            case _ => scanner (newRow::stack) (newRow) (n - 1)
        }
    }

    private def pascalsTriangle (k:Int):List[List[Int]] =
    {
        k match
        {
            case 0 => Nil
            case 1 => List(List(1))
            case _ => scanner (List(List(1))) (List(1)) (k - 1)
        }
    }

    def rows (numberOfRows:Int):List[List[Int]] =
    {
        numberOfRows > -1 match
        {
            case true => pascalsTriangle(numberOfRows)
            case false => Nil
        }
    }
}