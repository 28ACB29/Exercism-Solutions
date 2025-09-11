object Etl
{
  private def extractLoad(score: Int, letters: Seq[String]) =
  {
    letters.map[(String, Int)]((letter: String) => (letter.toLowerCase, score))
  }

  def transform(scoreMap: Map[Int, Seq[String]]): Map[String, Int] =
  {
    scoreMap.flatMap[String, Int](Function.tupled(extractLoad))
  }
}
