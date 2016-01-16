case class Bst(root: Int, left: Option[Bst] = None, right: Option[Bst] = None) {

  def value: Int = root

  def insert(value: Int): Bst = {
    def shouldInsertToLeft(value: Int): Boolean = {
      value <= this.value
    }

    if (shouldInsertToLeft(value)) {
      left match {
        case Some(bst) => Bst(this.value, Some(bst.insert(value)), right)
        case None => Bst(this.value, Some(Bst(value)), right)
      }
    }
    else {
      right match {
        case Some(bst) => Bst(this.value, left, Some(bst.insert(value)))
        case None => Bst(this.value, left, Some(Bst(value)))
      }
    }
  }
}

object Bst {
  def fromList(list: List[Int]): Bst = list.tail.foldLeft(Bst(list.head))((bst, i) => bst.insert(i))

  def toList(tree: Bst): List[Int] = tree match {
    case Bst(root, None, None) => List(root)
    case Bst(root, Some(left), None) => toList(left) :+ root
    case Bst(root, None, Some(right)) => root :: toList(right)
    case Bst(root, Some(left), Some(right)) => toList(left) ++ List(root) ++ toList(right)
  }
}