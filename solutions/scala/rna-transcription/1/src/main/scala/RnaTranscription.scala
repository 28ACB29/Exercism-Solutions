object RnaTranscription
{
    private def bases:Array[Char] = Array('G', 'C', 'T', 'A')

    private def validate(dna: String):Option[String] =
    {
        dna.forall(bases.contains) match
        {
            case true => Some(dna)
            case false => None
        }
    }

    private def transciber(nucleotide:Char):Char =
    {
        nucleotide match
        {
            case 'G' => 'C'
            case 'C' => 'G'
            case 'T' => 'A'
            case 'A' => 'U'
        }
    }

    def toRna(dna: String):Option[String] = validate(dna).map(valid => valid.map(transciber))
}