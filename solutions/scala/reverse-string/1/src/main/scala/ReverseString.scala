object ReverseString
{
  def reverse(str: String): String =
  {
    new String(str.toCharArray().reverse)
  }
}
