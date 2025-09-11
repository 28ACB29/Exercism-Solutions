enum RNASequence
{
    case Methionine
    case Phenylalanine
    case Leucine
    case Serine
    case Tyrosine
    case Cysteine
    case Tryptophan
    case STOP
}

object ProteinTranslation
{

    private def RNATranslation (codon:Array[Char]):RNASequence =
        codon match
        case Array('A', 'U', 'G')
            => RNASequence.Methionine
        case Array('U', 'U', 'U')
        | Array('U', 'U', 'C')
            => RNASequence.Phenylalanine
        case Array('U', 'U', 'A')
        | Array('U', 'U', 'G')
            => RNASequence.Leucine
        case Array('U', 'C', 'U')
        | Array('U', 'C', 'C')
        | Array('U', 'C', 'A')
        | Array('U', 'C', 'G')
            => RNASequence.Serine
        case Array('U', 'A', 'U')
        | Array('U', 'A', 'C')
            => RNASequence.Tyrosine
        case Array('U', 'G', 'U')
        | Array('U', 'G', 'C')
            => RNASequence.Cysteine
        case Array('U', 'G', 'G')
            => RNASequence.Tryptophan
        case Array('U', 'A', 'A')
        | Array('U', 'A', 'G')
        | Array('U', 'G', 'A')
            => RNASequence.STOP

    private def toString (rnaSequence:RNASequence):String =
        rnaSequence match
        case RNASequence.Methionine => "Methionine"
        case RNASequence.Phenylalanine => "Phenylalanine"
        case RNASequence.Leucine => "Leucine"
        case RNASequence.Serine => "Serine"
        case RNASequence.Tyrosine => "Tyrosine"
        case RNASequence.Cysteine => "Cysteine"
        case RNASequence.Tryptophan => "Tryptophan"
        case RNASequence.STOP => ""

    def proteins (rna:String):Seq[String] = rna.toCharArray().grouped(3).map(RNATranslation).takeWhile((rnaSequence:RNASequence) => rnaSequence != RNASequence.STOP).map(toString).toSeq
}