package p99

object MyLists {
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
    def elem0[S](list: List[S], cur: Int, till: Int): S =
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
    def reverse0[S](list: List[S], cur: List[S]): List[S] = list match {
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
    def append0[S](list1: List[S], list2: List[S]): List[S] = list2 match {
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
    def drop0[S](list: List[S], cur: Int, count: Int): List[S] = list match {
      case Nil => Nil
      case head :: tail =>
        if (cur == count)
        drop0(tail, 1, count)
      else
        head :: drop0(tail, cur + 1, count)
    }

    drop0(list, 1, count)
  }

  // 1.17
  def split[T](list: List[T], len: Int) = {
    def split0[S](list: List[S], cur: Int, len: Int): (List[S], List[S]) =
      if (cur == len)
        (Nil, list)
      else {
        val (list1, list2) = split0(list.tail, cur + 1, len)
        (list.head :: list1, list2)
      }

    split0(list, 0, len)
  }

  // 1.18
  def slice[T](list: List[T], from: Int, to: Int) = {
    def slice0[S](list: List[S], cur: Int, from: Int, to: Int): List[S] =
      if (cur < from)
        slice0(list.tail, cur + 1, from, to)
      else if (cur <= to)
        list.head :: slice0(list.tail, cur + 1, from, to)
      else
        Nil

    slice0(list, 1, from, to)
  }

  // 1.19
  def rotate[T](list: List[T], len: Int) =
    if (len == 0) list
    else if (len > 0) {
      val (list1, list2) = split(list, len)
      append(list2, list1)
    } else {
      val (list1, list2) = split(list, length(list) + len)
      append(list2, list1)
    }

  // 1.20
  def removeAt[T](list: List[T], i: Int) = {
    val (list1, list2) = split(list, i - 1)
    (append(list1, list2.tail), list2.head)
  }

  // 1.21
  def insertAt[T](list: List[T], e: T, i: Int) = {
    val (list1, list2) = split(list, i - 1)
    append(list1, e :: list2)
  }

  // 1.22
  def range(from: Int, to: Int): List[Int] =
    if (from == to)
      List(to)
    else
      from :: range(from + 1, to)

  // 1.23
  def rndSelect[T](list: List[T], count: Int) = {
    def rndSelect0[S](list1: List[S], list2: List[S],
      cur: Int, count: Int): List[S] =
      if (cur > count)
        list2
      else {
        val len = length(list1)
        val index = scala.util.Random.nextInt(len) + 1
        val (list3, e) = removeAt(list1, index)
        rndSelect0(list3, e :: list2, cur + 1, count)
      }

    rndSelect0(list, Nil, 1, count)
  }

  // 1.24
  def lotto[T](count: Int, to: Int) =
    rndSelect(range(1, to), count)

  // 1.25
  def rndPermu[T](list: List[T]) =
    rndSelect(list, length(list))

  // 1.26
  def combination[T](list: List[T], count: Int): Stream[List[T]] =
    if (count == 0)
      Stream(Nil)
    else {
      import scala.language.postfixOps
      (0 to (length(list) - count)).toStream map { i =>
        val (_, list1) = split(list, i + 1);
        val list2 = combination(list1, count - 1)
        list2.map(x => list(i) :: x)
      } flatten
    }

  // 1.27
  def group3[T](list: List[T]) = {
    for {
      g1 <- combination(list, 2)
      list1 = list diff g1
      g2 <- combination(list1, 3)
      g3 = list1 diff g2
    } yield List(g1, g2, g3)
  }

  def group[T](list: List[T], groups: List[Int]): Stream[List[List[T]]] = (groups: @unchecked) match {
    case _ :: Nil => Stream(List(list))
    case head :: tail =>
      combination(list, head).flatMap { x => 
        val list1 = list diff x
        group(list1, tail).map { y =>
          x :: y
        }
      }
  }
}
