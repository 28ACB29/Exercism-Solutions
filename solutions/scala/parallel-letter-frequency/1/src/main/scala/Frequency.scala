object Frequency
{
  def frequency(numWorkers:Int, texts:Seq[String]):Map[Char, Int] =
  {
    texts.mkString("").toCharArray().filter(Character.isLetter).map[Char](Character.toLowerCase).groupBy[Char](identity).map[Char, Int]((letter:Char, occurences:Array[Char]) => (letter, occurences.length))
  }
}
