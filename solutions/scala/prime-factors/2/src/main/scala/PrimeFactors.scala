object PrimeFactors
{

    private def factorize (stack:List[Long], number:Long, primeFactor:Long):List[Long] =
    {
        number match
        {
            case 1L => stack.reverse
            case _ =>
                number % primeFactor match
                {
                    case 0L => factorize(primeFactor::stack, number / primeFactor, primeFactor)
                    case _ => factorize(stack, number,primeFactor + 1)
                }
        }
    }

    def factors (number:Long):List[Long] =
    {
        number match
        {
            case 1 => Nil
            case _ => factorize(Nil, number, 2L)
        }
    }
}