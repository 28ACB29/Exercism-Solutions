object Darts
{
    private def inner:Double = 1.0

    private def middle:Double = 5.0

    private def outer:Double = 10.0

    private def radius (x:Double, y:Double):Double =
        Math.sqrt(x * x + y * y)

    def score(x:Double, y:Double):Int =
    {
        def distance:Double = radius(x, y)
        distance match
        case distance if distance > outer => 0
        case distance if distance > middle => 1
        case distance if distance > inner => 5
        case _ => 10
    }
}