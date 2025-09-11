object ArmstrongNumbers
{
    private def builder(digits:List[Int], remainder:Int):List[Int] =
    {
        remainder match
        {
            case 0 => digits
            case _ => builder((remainder % 10)::digits, remainder / 10)
        }
    }

    private def numberToDigits(number:Int):List[Int] =
    {
        builder(List.empty, number)
    }

    private def pown(number:Int, exponent:Int):Int =
    {
        exponent match
        {
            case 0 => 1
            case 1 => number
            case _ =>
                exponent % 2 == 0 match
                {
                    case true => pown(number * number, exponent / 2)
                    case false => number * pown(number * number, (exponent - 1) / 2)
                }
        }
    }

    private def sumPower(digits:List[Int]):Int =
    {
        def length:Int = digits.length
        digits.map[Int]((digit:Int) => pown (digit, length)).fold[Int](0)((x:Int, y:Int) => x + y)
    }

    def isArmstrongNumber (number:Int):Boolean =
    {
        sumPower(numberToDigits(number)) == number
    }
}