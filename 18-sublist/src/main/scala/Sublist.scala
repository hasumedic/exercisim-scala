class Sublist {
  def sublist[T](first: List[T], second: List[T]): Int = {
    if (first == second) Sublist.Equal
    else if (first.containsSlice(second)) Sublist.Superlist
    else if (second.containsSlice(first)) Sublist.Sublist
    else Sublist.Unequal
  }
}

object Sublist {
  val Equal = 0
  val Sublist = 1
  val Superlist = 2
  val Unequal = 3
}
