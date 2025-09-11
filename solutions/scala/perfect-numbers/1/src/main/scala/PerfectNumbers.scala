enum NumberType
{
    case Perfect
    case Abundant
    case Deficient
}

object PerfectNumbers
{

    private def accumulateFactors(n:Int, accumulator:Int, number:Int):Int =
    {
        n % number match
        {
            case 0 =>
                number == n / number match
                case true => accumulator + number
                case false => accumulator + number + (n / number)
            case _ => accumulator
        }
    }

    private def aliquotSum(n:Int):Int =
    {
        n match
        {
            case 1 => 0
            case _ =>
                val ceiling:Int = (Math.sqrt(n.toDouble)).toInt
                (2 to ceiling).foldLeft(1)((accumulator:Int, number:Int) => accumulateFactors(n, accumulator, number))
        }
    }


    def classify(n:Int):Either[String, NumberType] =
    {
        n match
        {
            case n if n > 0 =>
                val numberType:NumberType =
                    Math.signum(n.compareTo(aliquotSum(n))) match
                    {
                        case -1 => NumberType.Abundant
                        case 0 => NumberType.Perfect
                        case 1 => NumberType.Deficient
                    }
                Right(numberType)
            case _ => Left("Classification is only possible for natural numbers.")
        }
    }

}