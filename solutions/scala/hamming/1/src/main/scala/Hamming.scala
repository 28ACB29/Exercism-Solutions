object Hamming
{
  private def dissimilar[A] (a: A, b: A):Int =
  a == b match
  {
    case true => 0
    case false => 1
  }

  def distance(dnaStrandOne: String, dnaStrandTwo: String): Option[Int] =
  {
    def bases1: Array[Char] = dnaStrandOne.toCharArray()
    def bases2: Array[Char] = dnaStrandTwo.toCharArray()
    bases1.length == bases2.length match
    {
      case true => Some(bases1.zip[Char](bases2).map[Int](Function.tupled(dissimilar[Char])).sum[Int])
      case false => None
    }
  }
}
