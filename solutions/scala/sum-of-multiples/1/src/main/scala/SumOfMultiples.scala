object SumOfMultiples
{
  def sum(factors: Set[Int], limit: Int): Int =
  {
    
    def generateMultiples(n: Int): List[Int] =
    {
      val inclusiveNumber: Int = limit / n
      val boundary: Int =
        limit % n == 0 match
        {
          case true => 1
          case false => 0
        }
      val exclusiveNumber: Int = inclusiveNumber - boundary
      List.tabulate(exclusiveNumber)(i => (i + 1) * n)
    }
    factors.flatMap(generateMultiples).sum()
  }
}

