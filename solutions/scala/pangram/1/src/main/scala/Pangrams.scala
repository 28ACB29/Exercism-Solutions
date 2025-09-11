object Pangrams
{

  private val alphabet:Array[Char] = ('a' to 'z').toArray

  def isPangram(input: String): Boolean =
  {    
    input.toLowerCase().toCharArray().distinct.filter(Character.isLetter).sorted().equals(alphabet)
  }
}

