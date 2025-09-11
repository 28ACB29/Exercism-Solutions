object Isogram
{
    def isIsogram (str:String):Boolean =
    {
        val characters:Array[Char] = str.toLowerCase().toCharArray().filter(Character.isLetter)
        val length:Int = characters.length
        val distinctLength:Int = characters.distinct.length
        length == distinctLength
    }
}