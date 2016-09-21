package p99

object Arithmetic {
  // 2.01
  def isPrime(n: Int) = n match {
    case 1 => false
    case 2 => true
    case _ => !(2 to Math.sqrt(n.toDouble).toInt exists { n % _ == 0 })
  }

  // 2.02
  def primeFactors(n: Int): List[Int] = {
    def nextPrimeFactor(n: Int, cur: Int): Int = {
      if (isPrime(cur) && n % cur == 0)
        cur
      else
        nextPrimeFactor(n, cur + 1)
    }

    n match {
      case 1 => Nil
      case _ =>
        val f = nextPrimeFactor(n, 2)
        f :: primeFactors(n / f)
    }
  }

  // 2.03
  def primeFactorsMult(n: Int) = {
    MyLists.encode(primeFactors(n))
  }

  // 2.04
  def primesUpTo(n: Int) = {
    val isPrime = collection.mutable.BitSet(2 to n: _*) -- (4 to n by 2)
    for (p <- 2 +: (3 to Math.sqrt(n.toDouble).toInt by 2) if isPrime(p)) {
      isPrime --= p*p to n by p
    }
    isPrime.toImmutable
  }

  def rangeOfPrimes(from: Int, to: Int) = {
    primesUpTo(to).dropWhile(_ < from)
  }

  // 2.05
  def goldbach(n: Int) = {
    val primes = primesUpTo(n - 2)
    primes.find { x =>
      primes.exists { x + _ == n }
    } map { x => 
      List(x, n - x)
    }
  }

  // 2.06
  def goldbachList(from: Int, to: Int) = {
    val nextEven = from % 2 + from
    nextEven to to by 2 map goldbach
  }

  def goldbach(n: Int, lower: Int) = {
    val primes = primesUpTo(n - 2)
    primes.find { x =>
      x < lower && 
      primes.exists { x + _ == n }
    } map { x =>
      List(x, n - x)
    }
  }

  def goldbachList(from: Int, to: Int, lower: Int) = {
    val nextEven = from % 2 + from
    nextEven to to by 2 map {
      x => (x, goldbach(x, lower))
    } filter { x => x._2 == None }
  }

  def gcd(x: Int, y: Int): Int = y match {
    case 0 => x
    case _ => gcd(y, x % y)
  }

  def coprime(x: Int, y: Int) = gcd(x, y) == 1
}
