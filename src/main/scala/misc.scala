package p99

object Misc {
  // 7.01
  def eightQueues = {
    def conflict(a: Array[Array[Boolean]], pos: (Int, Int)) = {
      def diagonal(a: Array[Array[Boolean]], pos: (Int, Int), step: (Int, Int)): Boolean = {
        if (pos._1 < 0 || pos._1 == a.length || pos._2 < 0 || pos._2 == a.length) false
        else
          a(pos._1)(pos._2) || diagonal(a, (pos._1 - step._1, pos._2 - step._2), step)
      }

      (0 until a.length).toList.exists(a(_)(pos._2)) ||
        (0 until a.length).toList.exists(a(pos._1)(_)) ||
      diagonal(a, pos, (-1, -1)) || diagonal(a, pos, (-1, 1)) ||
        diagonal(a, pos, (1, -1)) || diagonal(a, pos, (1, 1))
    }

    // print only
    def eightQueues0(a: Array[Array[Boolean]], row: Int): Unit = {
      if (row == a.length)
        printBoard(a)
      else
        for (i <- 0 until a.length)
          if (!conflict(a, (row, i))) {
            val b = a.map(_.clone)
            b(row)(i) = true
            eightQueues0(b, row + 1)
          }
    }

    // collect all
    def eightQueues1(a: Array[Array[Boolean]], row: Int): List[Array[Array[Boolean]]] = {
      if (row == a.length)
        List(a)
      else
        (0 until a.length).toList.filterNot(i => conflict(a, (row, i))).flatMap { i =>
          val b = a.map(_.clone)
          b(row)(i) = true
          eightQueues1(b, row + 1)
        }
    }

    def printBoard(a: Array[Array[Boolean]]): Unit = {
      println("board")
      for (i <- a) {
        for (j <- i)
          print(s"$j ")
        println()
      }
    }

    eightQueues1(Array.fill(25, 25)(false), 0)
  }

  // simpler data structure, not better performance though
  def eightQueues1 = {
    def conflict(a: Array[Int], row: Int, col: Int) = {
      (0 until row).toList.exists { i =>
        a(i) == col ||
        a(i) - col == row - i ||
        col - a(i) == row - i
      }
    }

    def eightQueues0(a: Array[Int], row: Int): List[Array[Int]] = {
      if (row == a.length)
        List(a)
      else 
        (0 until a.length).toList.filterNot(conflict(a, row, _)).flatMap { i =>
          val b = a.clone
          b(row) = i
          eightQueues0(b, row + 1)
        }
    }

    eightQueues0(new Array(25), 0)
  }
}
