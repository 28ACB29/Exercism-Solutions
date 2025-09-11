class Triangle(a:Double, b:Double, c:Double)
{
    private def isTriangle:Boolean = this.a + this.b > this.c && this.b + this.c > this.a && this.a + this.c > this.b

    def equilateral:Boolean = this.isTriangle && this.a == this.b && this.b == this.c

    def isosceles:Boolean = this.isTriangle && this.a == this.b || this.b == this.c || this.c == this.a

    def scalene:Boolean = this.isTriangle && this.a != this.b && this.b != this.c && this.c != this.a
}