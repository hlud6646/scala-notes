package scala 

object Arithmetic extends App {

  // 31. 
  def isPrime(n: Int) = (2 to math.pow(n, .5).toInt).forall(n % _ != 0)
  assert(isPrime(19) && !isPrime(15))

  // 32. GCD by Euclids algo. 
  def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)
  assert(gcd(12, 15) == gcd(15, 12) && gcd(12, 15) == 3)

  //33. Coprime predicate.
  def coprime(x: Int, y: Int) = gcd(x, y) == 1


}

