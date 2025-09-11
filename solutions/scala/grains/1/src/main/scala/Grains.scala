object Grains
{
    def square(n:Int):Option[BigInt] =
        n > 0 && n < 65 match
        {
            case true => Some(BigInt(2).pow(n - 1))
            case false => None
        }

    def total:BigInt = Seq.range(0, 64).map((n:Int) => BigInt(2).pow(n)).sum
}