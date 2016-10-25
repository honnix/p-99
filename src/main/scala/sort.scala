package p99

// not from the original p-99
object Sort {
  // quicksort
  def quicksort(a: Array[Int]): Unit = {
    def partition(a: Array[Int], lo: Int, hi: Int) = {
      def partition0(a: Array[Int], i: Int, j: Int, hi: Int): Int = {
        def swap0(a: Array[Int], i: Int, j: Int) = {
          val t = a(i)
          a(i) = a(j)
          a(j) = t
        }

        if (j == hi) {
          swap0(a, i, hi)
          i
        } else {
          val i1 = if (a(j) <= a(hi)) {
            swap0(a, i, j)
            i + 1
          } else i
          partition0(a, i1, j + 1, hi)
        }
      }

      partition0(a, lo, lo, hi)
    }

    def quicksort0(a: Array[Int], lo: Int, hi: Int): Unit = {
      if (lo < hi) {
        val p = partition(a, lo, hi)
        quicksort0(a, lo, p - 1)
        quicksort0(a, p + 1, hi)
      }
    }

    quicksort0(a, 0, a.length - 1)
  }

  // merge sort
  def mergeSort(a: Array[Int]) = {
    def mergeSort0(a: Array[Int], lo: Int, hi: Int): Array[Int] = {
      def merge(a: Array[Int], a1: Array[Int], a2: Array[Int], i: Int, j: Int, k: Int): Unit = {
        if (i != a1.length || j != a2.length)
          if (i == a1.length || j != a2.length && a1(i) >= a2(j)) {
            a(k) = a2(j)
            merge(a, a1, a2, i, j + 1, k + 1)
          } else {
            a(k) = a1(i)
            merge(a, a1, a2, i + 1, j, k + 1)
          }
      }

      if (lo == hi) Array(a(lo))
      else {
        val mid = (hi - lo) / 2 + lo
        val a1 = mergeSort0(a, lo, mid)
        val a2 = mergeSort0(a, mid + 1, hi)
        val a3 = new Array[Int](hi - lo + 1)
        merge(a3, a1, a2, 0, 0, 0)
        a3
      }
    }

    mergeSort0(a, 0, a.length - 1)
  }

  // insertion sort
  def insertionSort(a: Array[Int]): Unit = {
    for (i <- 1 until a.length) {
      val x = a(i)
      var j = i - 1
      while (j >= 0 && a(j) > x) {
        a(j + 1) = a(j)
        j -= 1
      }
      a(j + 1) = x
    }
  }
}
