object Strain
{
    def keep[A](xs:Seq[A], pred:A => Boolean):Seq[A] =
    {
        for (x <- xs if pred(x))
            yield x
    }

    def discard[A](xs:Seq[A], pred:A => Boolean):Seq[A] =
    {
        for (x <- xs if !pred(x))
            yield x
    }
}