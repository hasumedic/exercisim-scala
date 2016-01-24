case class Deque[T]() {
  private var lastElements: List[T] = List()
  private var firstElements: List[T] = List()

  // Push and Pop operate on lastElements
  def push(element: T): Unit = {
    lastElements = element :: lastElements
  }

  def pop(): Option[T] = {
    if (lastElements.isEmpty) {
      lastElements = firstElements.reverse
      firstElements = List()
      pop()
    }
    else {
      val element = lastElements.head
      lastElements = lastElements.tail
      Some(element)
    }
  }

  //Shift and unshift operate on firstElements
  def shift(): Option[T] = {
    if (firstElements.isEmpty) {
      firstElements = lastElements.reverse
      lastElements = List()
      shift()
    }
    else {
      val element = firstElements.head
      firstElements = firstElements.tail
      Some(element)
    }
  }

  def unshift(element: T): Unit = {
    firstElements = element :: firstElements
  }
}
