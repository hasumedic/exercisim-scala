object Allergen extends Enumeration {

  type Allergen = Value

  val Eggs = Value(1)
  val Peanuts = Value(2)
  val Shellfish = Value(4)
  val Strawberries = Value(8)
  val Tomatoes = Value(16)
  val Chocolate = Value(32)
  val Pollen = Value(64)
  val Cats = Value(128)
}

case class Allergies() {

  import Allergen._

  def isAllergicTo(allergen: Allergen, number: Int): Boolean = {
    (allergen.id & number) != 0
  }

  def allergies(number: Int): List[Allergen] = {
    Allergen.values.filter(a => isAllergicTo(a, number)).toList
  }
}









