package ninetyNine

object BinaryTrees extends App {
  
  trait Tree[+T]
  case object Empty extends Tree[Nothing]
  case class Leaf[+T](v: T) extends Tree[T]
  case class Fork[+T](v: T, l: Tree[T], r: Tree[T]) extends Tree[T] 
  object Tree {

    private def flip[T](t: Tree[T]) = t match {
      case Empty | Leaf(_) => t
      case Fork(v, l, r)   => Fork(v, r, l)
    }

    /**
     * Construct a completly balanced binary tree with n nodes, each 
     * with the value t.
     */
    def cBalanced[T](n: Int, v: T): Tree[T] = 
      if (n == 0) Empty else if (n == 1) Leaf(v) else {
        val midpoint = n / 2
        val l = cBalanced(midpoint, v)
        val r = cBalanced(n - midpoint, v)
        Fork(v, l, r)
      }

    def isSymmetric[T](tree: Tree[T]) = tree match {
      case Empty | Leaf(_) => true
      case Fork(v, l, r)   => l == flip(r) 
    }

  }
  
  Fork('a', Leaf('b'), Leaf('c')).isSymmetric

}
