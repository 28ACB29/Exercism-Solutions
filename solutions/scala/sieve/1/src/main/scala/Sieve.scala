object Sieve
{

    private def sieve(stack:List[Int], numbers:List[Int]):List[Int] =
        numbers match
        case Nil => stack.reverse
        case (head:Int)::(tail:List[Int]) => sieve(head::stack, tail.filter((element:Int) => element % head != 0))

    def primes(limit:Int):List[Int] = sieve(Nil, (2 to limit).toList)
}