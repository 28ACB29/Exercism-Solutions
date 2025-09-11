object Acronym
{
  def abbreviate(phrase: String): String =
  {
    String(phrase.toUpperCase().split("[ -]").filter((word:String) => word.length > 0).map((word:String) => word.toCharArray()(0)))
  }
}
