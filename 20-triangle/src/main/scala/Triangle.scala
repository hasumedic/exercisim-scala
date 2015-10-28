class Triangle(x: Int, y: Int, z: Int) {

  def triangleType(): String = {
    if (hasInvalidSides || (hasTwoEqualSides && thirdSideIsBigger)) TriangleType.Illogical
    else if (allEqualSides) TriangleType.Equilateral
    else if (nonEqualSides) TriangleType.Scalene
    else TriangleType.Isosceles
  }

  private def thirdSideIsBigger: Boolean = {
    if (x == y) z > x
    else if (x == z) y > x
    else x > y
  }

  private def nonEqualSides: Boolean = {
    x != y && y != z && x != z
  }

  private def allEqualSides: Boolean = {
    x == y && y == z
  }

  private def hasTwoEqualSides: Boolean = {
    x == y || y == z || x == z
  }

  private def hasInvalidSides: Boolean = {
    x < 1 || y < 1 || z < 1
  }

}

object Triangle {
  def apply(x: Int, y: Int, z: Int) = new Triangle(x, y, z)
}

object TriangleType {
  val Equilateral = "equilateral"
  val Isosceles = "isosceles"
  val Scalene = "scalene"
  val Illogical = "illogical"
}

