object AllYourBase
{

    private def ValidDigits(inputBase:Int, digits:List[Int]):Boolean = digits.forall((digit:Int) => digit > -1 && digit < inputBase)

    private def digitsToNumber(inputBase:Int, digits:List[Int]):Int =
    {
        digits match
        {
            case Nil => 0
            case _ => digits.foldLeft(0)((number:Int, digit:Int) => number * inputBase + digit) 
        }
    }

    private def numberTodigits(outputBase:Int, number:Int):List[Int] =
    {
        def tailCall (digits:List[Int]) (remainder:Int):List[Int] =
        {
            remainder match
            {
                case 0 => digits
                case _ => tailCall ((remainder % outputBase)::digits) (remainder / outputBase)
            }
        }
        def digits:List[Int] = tailCall (Nil) (number)
        digits match
        {
            case Nil => List(0)
            case _ => digits
        }
    }

    def rebase(inputBase:Int, digits:List[Int], outputBase:Int):Option[List[Int]] =
    {
        inputBase match
        {
            case inputBase if inputBase > 1 =>
                outputBase match
                case outputBase if outputBase > 1 =>
                    digits match
                    case digits if ValidDigits(inputBase, digits) =>
                        Some(numberTodigits(outputBase, digitsToNumber(inputBase, digits)))
                    case _ => None
                case _ => None
            case _ => None
        }
    }
}