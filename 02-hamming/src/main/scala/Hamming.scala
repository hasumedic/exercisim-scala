object Hamming {
  def compute(from: String, to: String): Int = {
    if(from.length != to.length) throw new IllegalArgumentException
    (from zip to).count(chars => chars._1 != chars._2)
  }
}
