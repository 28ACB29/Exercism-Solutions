object SpaceAge
{

  private def yearsOnEarth(age: Double):Double = age / 31557600.toDouble

  def onEarth(age: Double): Double = yearsOnEarth(age)

  def onVenus(age: Double): Double = yearsOnEarth(age) / 0.61519726

  def onMercury(age: Double): Double = yearsOnEarth(age) / 0.2408467

  def onMars(age: Double): Double = yearsOnEarth(age) / 1.8808158

  def onJupiter(age: Double): Double = yearsOnEarth(age) / 11.862615

  def onSaturn(age: Double): Double = yearsOnEarth(age) / 29.447498

  def onUranus(age: Double): Double = yearsOnEarth(age) / 84.016846

  def onNeptune(age: Double): Double = yearsOnEarth(age) / 164.79132
}
