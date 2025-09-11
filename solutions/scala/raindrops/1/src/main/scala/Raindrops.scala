object Raindrops
{

  private def dictionary:Array[(Int, String)] = Array((3, "Pling"), (5, "Plang"), (7, "Plong"))

  private def appender(number:Int, buffer:String, factor:Int, word:String) =
    number % factor match
    case 0 => buffer + word
    case _ => buffer

  private def sentenceCreator (number:Int):String = dictionary.foldLeft[String]("")((buffer:String, entry:(Int, String)) => appender(number, buffer, entry._1, entry._2))

  def convert(n: Int): String =
  {
    val sentence:String = sentenceCreator(n)
    sentence match
    case "" => n.toString()
    case _ => sentence
  }
}

