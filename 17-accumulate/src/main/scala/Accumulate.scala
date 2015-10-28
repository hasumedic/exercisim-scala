class Accumulate {
  def accumulate[A, B](f: A => B, list: List[A]) = list.map(f)
}
