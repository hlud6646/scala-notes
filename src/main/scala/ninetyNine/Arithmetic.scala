package scala 

object Arithmetic extends App {

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
  def primeFactors(n: Int) = (2 to n) filter {n % _ == 0} filter isPrime
  
  // 36.
  def primeFactorMultiplicity(n: Int) = {
    def help(n: Int): List[Int] = 
      if (n == 1) List() else {
      val x = primeFactors(n).toList
      val y = x.fold(n)(_/_)
      x ::: help(y) 
      }
    help(n).groupBy(identity).mapValues(_.size)
  }
  println(primeFactorMultiplicity(315))
}

