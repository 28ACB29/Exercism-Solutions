object CollatzConjecture
{

    private def nextNumber (n: Integer): Integer =
    {
        n % 2 match
        {
            case 0 => n / 2
            case 1 => 3 * n + 1
        }
    }

    private def countSteps (n: Integer, i: Integer): (Integer, Integer) =
    {
        n == 1 match
        {
            case true => (1, i)
            case false => countSteps(nextNumber(n), i + 1)
        }
    }

    def steps (number: Integer): Option[Integer] =
    {
        number < 1 match
        {
            case true => None
            case false => Some(countSteps(number, 0)._2)
        }
    }
}