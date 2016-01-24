case class CustomSet[T](list: List[T])

object CustomSet {

  def fromList[T](list: List[T]): CustomSet[T] = new CustomSet[T](list)

  def toList[T](set: CustomSet[T]): List[T] = set.list

  def empty[T](set: CustomSet[T]): Boolean = set.list.isEmpty

  def singleton[T](set: CustomSet[T]): Boolean = set.list.size == 1

  def size[T](set: CustomSet[T]): Int = set.list.size

  def member[T](set: CustomSet[T], element: T): Boolean = set.list.contains(element)

  def insert[T](set: CustomSet[T], element: T): CustomSet[T] = {
    if (CustomSet.member(set, element)) set
    else CustomSet.fromList(element :: set.list)
  }

  def delete[T](set: CustomSet[T], element: T): CustomSet[T] = {
    if (CustomSet.member(set, element)) CustomSet.fromList(set.list.diff(List(element)))
    else set
  }

  def union[T](first: CustomSet[T], second: CustomSet[T]): CustomSet[T] = {
    CustomSet.fromList((first.list ++ second.list).distinct)
  }

  def difference[T](first: CustomSet[T], second: CustomSet[T]): CustomSet[T] = {
    val bothDiffs = first.list.diff(second.list) ++ second.list.diff(first.list)
    CustomSet.fromList(bothDiffs.distinct)
  }

  def intersection[T](first: CustomSet[T], second: CustomSet[T]): CustomSet[T] = {
    CustomSet.fromList(first.list.filter(CustomSet.member(second, _)))
  }

  def isSubsetOf[T](first: CustomSet[T], second: CustomSet[T]): Boolean = {
    first.list.forall(CustomSet.member(second, _))
  }

  def isDisjointFrom[T](first: CustomSet[T], second: CustomSet[T]): Boolean = {
    first.list.forall(!CustomSet.member(second, _))
  }
}

