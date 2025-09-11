class DNA(strand:String)
{
    private def nucleotides:Array[Char] = Array[Char]('A', 'C', 'G', 'T')

    private def onlyNucleotides (strand:String):Boolean = strand.toCharArray().forall((character:Char) => nucleotides.contains(character))

    private def emptyCount:Right[Unit, Map[Char, Int]] = Right(Map('A' -> 0, 'C' -> 0, 'G' -> 0, 'T' -> 0))

    def nucleotideCounts:Either[Unit, Map[Char, Int]] =
        onlyNucleotides(this.strand) match
        {
            case true =>
                this.strand match
                {
                    case "" => emptyCount
                    case _ =>
                        def chars:Array[Char] = this.strand.toCharArray()
                        Right(Map('A' -> chars.count((n:Char) => n == 'A'), 'C' -> chars.count((n:Char) => n == 'C'), 'G' -> chars.count((n:Char) => n == 'G'), 'T' -> chars.count((n:Char) => n == 'T')))
                }
            case false => Left(())
        }
}