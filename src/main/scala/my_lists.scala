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
      else
        elem0(list.tail, cur + 1, till)

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

  def flatten[T](list: List[_]): List[T] = list match {
    case Nil => Nil
    case head :: tail => head match {
      case l: List[_] => append(flatten(l), flatten(tail))
      case _ => head.asInstanceOf[T] :: flatten(tail)
    }
  }

  // 1.08
  def compress[T](list: List[T]) = {
    def compress0(list1: List[T], list2: List[T], cur: T): List[T] = list1 match {
      case Nil => list2
      case head :: tail if head == cur => compress0(tail, list2, head)
      case _ => compress0(list1.tail, list1.head :: list2, list1.head)
    }

    reverse(compress0(list.tail, List(list.head), list.head))
  }

  // 1.09
  def pack[T](list: List[T]) = {
    def pack0(list1: List[T], list2: List[List[T]], cur: T): List[List[T]] = list1 match {
      case Nil => list2
      case head :: tail if head == cur => pack0(tail, (head :: list2.head) :: list2.tail, head)
      case _ => pack0(list1.tail, List(list1.head) :: list2, list1.head)
    }

    reverse(pack0(list.tail, List(List(list.head)), list.head))
  }

  // 1.10
  def encode[T](list: List[T]) = {
    val list1 = pack(list)
    list1.map(x => List(length(x), x.head))
  }

  // 1.11
  def encode_modified[T](list: List[T]) = {
    val list1 = pack(list)
    list1.map {
      case x :: Nil => x
      case x => List(length(x), x.head)
    }
  }

  // 1.12
  def decode[T](list: List[_]): List[T] = {
    val list1 = list.map {
      case count :: elem :: Nil => List.fill(count.asInstanceOf[Int])(elem)
      case x => x
    }
    flatten(list1)
  }

  // 1.13
  def encode_direct[T](list: List[T]) = {
    def encode_direct0(list1: List[T], list2: List[_], cur: T): List[_] = list1 match {
      case Nil => list2
      case head :: tail if head == cur => list2.head match {
        case count :: elem :: Nil => encode_direct0(tail, List(count.asInstanceOf[Int] + 1, elem) :: list2.tail, cur)
        case elem => encode_direct0(tail, List(2, elem) :: list2.tail, cur)
      }
      case _ => encode_direct0(list1.tail, list1.head :: list2, list1.head)
    }

    reverse(encode_direct0(list.tail, List(list.head), list.head))
  }

  // 1.14
  def dupli[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case head :: tail => head :: head :: dupli(tail)
  }

  // 1.15
  def dupli[T](list: List[T], count: Int): List[T] = list match {
    case Nil => Nil
    case head :: tail => append(List.fill(count)(head), dupli(tail, count))
  }

  // 1.16
  def drop[T](list: List[T], count: Int) = {
    def drop0[T](list: List[T], cur: Int, count: Int): List[T] = list match {
      case Nil => Nil
      case head :: tail =>
        if (cur == count)
        drop0(tail, 1, count)
      else
        head :: drop0(tail, cur + 1, count)
    }

    drop0(list, 1, count)
  }
}
