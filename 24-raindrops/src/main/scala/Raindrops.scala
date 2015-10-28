case class Raindrops() {
  def convert(input: Int): String = {
    var output: String = ""
    if (input % 3 == 0) output += "Pling"
    if (input % 5 == 0) output += "Plang"
    if (input % 7 == 0) output += "Plong"
    if (output.isEmpty) output += input.toString

    output
  }
}
