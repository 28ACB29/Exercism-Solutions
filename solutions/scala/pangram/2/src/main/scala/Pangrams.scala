object Pangrams
{

  private val alphabet:Set[Char] = ('a' to 'z').toSet

  def isPangram(input: String): Boolean =
  {    
    input.toLowerCase().toCharArray().filter(Character.isLetter).toSet.equals(alphabet)
  }
}

