package scala

object Arithmetic {

  // 31.
  def isPrime(n: Int) = (2 to math.pow(n, .5).toInt).forall(n % _ != 0)
  assert(isPrime(19) && !isPrime(15))

  // 32.
  def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)
  assert(gcd(12, 15) == gcd(15, 12) && gcd(12, 15) == 3)

  // 33.
  def coprime(x: Int, y: Int) = gcd(x, y) == 1

  // 34.
  def phi(n: Int) = (1 to n).map(coprime(_, n)).count(_ == true)
  assert(phi(10) == 4)

  // 35.
  def primeFactors(n: Int) = (2 to n) filter { n % _ == 0 } filter isPrime

  // 36.
  def primeFactorMultiplicity(n: Int) = {
    def help(n: Int): List[Int] =
      if (n == 1) List()
      else {
        val x = primeFactors(n).toList
        val y = x.fold(n)(_ / _)
        x ::: help(y)
      }
    help(n).groupBy(identity).mapValues(_.size)
  }

  // 37.
  def totient(n: Int) = primeFactorMultiplicity(n)
    .map(kv => math.pow(kv._1, (kv._2 - 1)).toInt * (kv._1 - 1))
    .product

  // 39.
  def listPrimes(a: Int, b: Int) = (2 to b) filter isPrime

  // 40. Goldbach's conjecture: Every odd even number greater than 2 can
  // be writen as the sum of two primes.
  def goldbach(n: Int) = {
    val p = listPrimes(3, n)
    (for {
      a <- p
      b <- p if a + b == n
    } yield (a, b)).head
  }

  // 41. Print the Goldbach pair for all even numbers in a given range.
  // No guards or anything on this; dies if a == 2 or either is odd.
  def printGoldbach(a: Int, b: Int) = (a to b by 2).map(goldbach).foreach {
    case (x, y) => println(s"${x + y} = $x + $y")
  }
  def printGoldbachLimited(a: Int, b: Int, m: Int) =
    (a to b by 2)
      .map(goldbach)
      .map(t => List(t._1, t._2))
      .filter(_.min >= m) foreach { t =>
      println(s"${t(0) + t(1)} = $t(0) + $t(1)")
    }

}
