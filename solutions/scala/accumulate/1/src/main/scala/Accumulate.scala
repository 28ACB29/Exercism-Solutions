class Accumulate
{

  private def builder[A, B](stack:List[B], func: A => B, list:List[A]):List[B] =
    list match
    {
      case Nil => stack.reverse
      case (head: A)::(tail: List[A]) => builder(func(head)::stack, func,tail)
    }

  def accumulate[A, B](f: (A) => B, list : List[A]): List[B] = builder(Nil, f, list)
}
