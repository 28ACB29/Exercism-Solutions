object DifferenceOfSquares
{

  private def square (n:Int):Int = n * n

  def sumOfSquares(n: Int): Int = n * (n + 1) * (2 * n + 1) / 6

  def squareOfSum(n: Int): Int = square(Seq.range(1, n + 1).sum())

  def differenceOfSquares(n: Int): Int =  squareOfSum(n) - sumOfSquares(n)
}
