case class AnimalVerse(animal: String, caughtAnimal: String, verse: String)

object FoodChain {
  private val verseSeparator = "\n\n"
  private val doNotKnow = "I don't know why she swallowed the fly. Perhaps she'll die."
  val finalVerse = List(
    "I know an old lady who swallowed a horse.",
    "She's dead, of course!"
  ).mkString("\n")

  val animals = List(
    AnimalVerse("cow", "cow", "I don't know how she swallowed a cow!"),
    AnimalVerse("goat", "goat", "Just opened her throat and swallowed a goat!"),
    AnimalVerse("dog", "dog", "What a hog, to swallow a dog!"),
    AnimalVerse("cat", "cat", "Imagine that, to swallow a cat!"),
    AnimalVerse("bird", "bird", "How absurd to swallow a bird!"),
    AnimalVerse("spider", "spider that wriggled and jiggled and tickled inside her", "It wriggled and jiggled and tickled inside her."),
    AnimalVerse("fly", "fly", "")
  )

  def song: String = {
    def generateVerses: String = {
      animals.foldLeft(("", animals)) { (acc, animalVerse) =>
        val currentAnimal = animalVerse.animal
        val animalList = acc._2
        val previousVerses = acc._1
        (List(
          s"I know an old lady who swallowed a $currentAnimal.",
          animalVerse.verse,
          generateInnerVerses(animalList.head, animalList.tail)
        ).filter(!_.isEmpty).mkString("\n") + previousVerses
          , animalList.tail)
      }._1
    }

    def generateInnerVerses(currentAnimal: AnimalVerse, restAnimals: List[AnimalVerse]): String = {
      if (restAnimals.isEmpty) doNotKnow + verseSeparator
      else {
        val swallowedAnimal = currentAnimal.animal
        val caughtAnimal = restAnimals.head.caughtAnimal
        s"She swallowed the $swallowedAnimal to catch the $caughtAnimal.\n" + generateInnerVerses(restAnimals.head, restAnimals.tail)
      }
    }

    generateVerses + finalVerse + verseSeparator
  }
}
