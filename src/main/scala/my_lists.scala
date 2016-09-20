object MyList {
  // 1.01
  def last[T](list: List[T]): T = list match {
    case last :: Nil => last
    case _ => last(list.tail)
  }

  // 1.02
  def lastButOne[T](list: List[T]): T = list match {
    case lastButOne :: _ :: Nil => lastButOne
    case _ => lastButOne(list.tail)
  }

  // 1.03
  def elem[T](list: List[T], index: Int) = {
    def elem0[T](list: List[T], cur: Int, till: Int): T =
      if (cur == till)
        list.head
      else {
        val next = cur + 1
        elem0(list.tail, next, till)
      }

    elem0(list, 0, index)
  }

  // 1.04
  def length[T](list: List[T]): Int = list match {
    case Nil => 0
    case _ :: tail => 1 + length(tail)
  }

  // 1.05
  def reverse[T](list: List[T]) = {
    def reverse0[T](list: List[T], cur: List[T]): List[T] = list match {
      case Nil => cur
      case head :: tail => val next = head :: cur; reverse0(tail, next)
    }

    reverse0(list, List())
  }

  // 1.06
  def isPalindrome[T](list: List[T]) =
    reverse(list) == list

  // 1.07
  def append[T](list1: List[T], list2: List[T]) = {
    def append0[T](list1: List[T], list2: List[T]): List[T] = list2 match {
      case Nil => list1
      case head :: tail => append0(head :: list1, tail)
    }

    reverse(append0(reverse(list1), list2))
  }

  def flatten[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case head :: tail => head match {
      case l: List[T] @unchecked => append(flatten(l), flatten(tail))
      case _ => head :: flatten(tail)
    }
  }
}
