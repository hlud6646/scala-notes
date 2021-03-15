package ninetyNine

object BinaryTrees {

  trait Tree[+T] {
    def flip: Tree[T]
    def size: Int
    def height: Int
    def leaves: List[Leaf[T]]
    def internal: List[Fork[T]]
    def atLevel(i: Int): List[Tree[T]]
    def isSymmetric: Boolean
    def isBalanced: Boolean
    def isHbalanced: Boolean
    def insert[U >: T <% Ordered[U]](x: U): Tree[U]
  }

  case object Empty extends Tree[Nothing] {
    override def toString = "."
    def flip = this
    def size = 0
    def height = 0
    def leaves = Nil
    def internal = Nil
    def atLevel(i: Int) = Nil
    def isSymmetric = true
    def isBalanced = true
    def isHbalanced = true
    def insert[U <% Ordered[U]](x: U) = Leaf(x)
  }

  case class Leaf[+T](value: T) extends Tree[T] {
    override def toString = this.value.toString
    def flip = this
    def size = 1
    def height = 1
    def leaves = this :: Nil
    def internal = Nil
    def atLevel(i: Int) = if (i == 1) this :: Nil else Nil
    def isSymmetric = true
    def isBalanced = true
    def isHbalanced = true
    def insert[U >: T <% Ordered[U]](x: U) =
      if (this.value == x) this
      else if (x < this.value) Fork(this.value, Leaf(x), Empty)
      else Fork(this.value, Empty, Leaf(x))
  }

  case class Fork[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    def flip = Fork(value, right.flip, left.flip)
    def size = 1 + left.size + right.size
    def height = left.height + right.height
    def leaves = left.leaves ::: right.leaves
    def internal = this match {
      case Fork(_, Fork(_, _, _), _) => this :: left.internal ::: right.internal
      case Fork(_, _, Fork(_, _, _)) => this :: left.internal ::: right.internal
      case _                         => Nil
    }
    def atLevel(i: Int) =
      if (i == 1) this :: Nil else left.atLevel(i - 1) ::: right.atLevel(i - 1)
    def isSymmetric = this == this.flip
    def isBalanced = left.size == right.size
    def isHbalanced = List(-1, 0, 1).contains(
      left.height - right.height
    ) && left.isHbalanced && right.isHbalanced
    def insert[U >: T <% Ordered[U]](x: U) =
      if (this.value == x) this
      else if (x < this.value) Fork(this.value, left.insert(x), right)
      else Fork(this.value, left, right.insert(x))
  }

  object Tree {

    /** Construct a completly balanced binary tree with n nodes, each
      * with the value t.
      */
    def cBalanced[T](n: Int, v: T): Tree[T] =
      if (n == 1) Leaf(v)
      else {
        val midpoint = n / 2
        val l = cBalanced(midpoint, v)
        val r = cBalanced(n - midpoint, v)
        Fork(v, l, r)
      }

    def fromList(x: List[Int]): Tree[Int] = x match {
      case Nil     => Empty
      case x :: xs => fromList(xs).insert(x)
    }

    /** Lazy list of lists of tress, grouped by number of nodes.
      *
      * First item is unique tree with zero nodes, namely the Empty tree.
      * Assume we know how to make every tree of size 0, 1, ..., n - 1.
      * Then every tree with n nodes is of the form
      *
      *      x
      *     / \
      *    A   B
      *
      * where x is new node, A has m nodes, and B has n - 1 - m nodes.
      */

    // This is a bit sad, since it computes everything over and over.
    def trees1(n: Int): List[Tree[String]] = if (n == 0) List(Empty)
    else
      (for {
        i <- 0 to n - 1
        a <- trees1(i)
        b <- trees1(n - 1 - i)
      } yield Fork("o", a, b)).toList

    // This is better? Feels pretty idiomatic, but I still want to see something like
    // ints.zip(ints.reverse).map{case (as, bs) => map2(Node("o", _, _)}.
    val ints: LazyList[Int] = 0 #:: ints.map(_ + 1)
    val allTrees: LazyList[List[Tree[String]]] = List(Empty) #:: ints
      .drop(1)
      .map(n => {
        (for {
          i <- 0 until n
          a <- allTrees(i)
          b <- allTrees(n - 1 - i)
        } yield Fork("o", a, b)).toList
      })

    /** This function seems like a perfect opportunity for some functional programming abuse.
      * Obviously, binary predicates of the same input type form a monoid with the && operation.
      * Thus we could combine isSymmetric and isBalanced to a single predicate and only
      * reference t once. That would be dumb tho, cause it's currently perfectly readable.
      */
    def symmetricBalancedTrees(n: Int) = allTrees(n) filter { t =>
      t.isSymmetric && t.isBalanced
    }
    def heightBalancedTrees(n: Int) = allTrees(n) filter { _.isHbalanced }
  }

}
