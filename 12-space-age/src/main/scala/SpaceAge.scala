case class SpaceAge(seconds: Long) {

  lazy val orbitalTime = seconds.toDouble / 31557600

  sealed abstract class Planet(orbitalPeriod: Double) {
    def getRatio: Double =
      BigDecimal(orbitalTime / orbitalPeriod).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  case object MERCURY extends Planet(0.2408467)
  case object VENUS extends Planet(0.615)
  case object EARTH extends Planet(1)
  case object MARS extends Planet(1.8808158)
  case object JUPITER extends Planet(11.862615)
  case object SATURN extends Planet(29.4)
  case object URANUS extends Planet(84.016846)
  case object NEPTUNE extends Planet(164.79132)

  def onEarth: Double = EARTH.getRatio

  def onMercury: Double = MERCURY.getRatio

  def onVenus: Double = VENUS.getRatio

  def onMars: Double = MARS.getRatio

  def onJupiter: Double = JUPITER.getRatio

  def onSaturn: Double = SATURN.getRatio

  def onUranus: Double = URANUS.getRatio

  def onNeptune: Double = NEPTUNE.getRatio
}
