package ninetyNine

/**
* These are the classical 99 Prolog Problems, done in Scala (obviously).
* Naive solutions are given, in an attempt to demonstrate some of the
* general features of the language as well as FP patterns.  After each of 
* these comes an assert statement where the naive solution is compared 
* to an idiomatic scala one.
*
* There are also tests, largely redundant but perhaps useful as a reference
* on a few testing libraries.
*/

object Lists {
  
  /**
  * 1. Find the last element of a list.
  */
  def last[T](items: List[T]): T = items match {
    case Nil       => throw new IllegalArgumentException("Empty list.")
    case x :: Nil  => x
    case x :: tail => last(tail)
  }
  assert(List(1, 2, 3).last == last(List(1, 2, 3)))

  /**
  * 2. Penultimate element of list.
  */
  def penultimate[T](items: List[T]): T = items match {
    case Nil      => throw new Exception
    case x :: Nil => throw new Exception
    case x :: y :: Nil => x
    case x :: tail     => penultimate(tail)
  }
  assert(List(1, 2, 3).dropRight(1).last == penultimate(List(1, 2, 3)))

  /**
  * 3. k-th element of list.
  * Lazily not worrying about what kind of exception is
  */
  def getAtIndex[T](i: Int, items: List[T]): T = i match {
    case 0 => items.head
    case n => getAtIndex(i - 1, items.tail)
  }
  assert(getAtIndex(2, List(1, 2, 3, 4)) == List(1, 2, 3, 4)(2))
  // refer to test/scala/lists/problems.scala for validation

  /**
  * 4. Number of elements.
  */
  def length[T](items: List[T]): Int = items match {
    case Nil => 0
    case x :: y => 1 + length(y)
  }
  assert(length(List(1, 2, 3, 4)) == 4)

  /**
  * 5. Reverse a list.
  */
  def reverse[T](items: List[T]): List[T] = items match {
    case Nil      => Nil
    case x :: Nil => List(x)
    case x :: y   => reverse(y) :+ x
  }
  assert(reverse(List(1, 2, 3)) == List(3, 2, 1))

  /**
  * 6. Palindrome predicate.
  */
  def isPalindrome[T](x: List[T]) = x == reverse(x)
  assert(isPalindrome(List(1, 2, 3, 2, 1)))
  assert(!isPalindrome(List(1, 2)))


  /**
  * 7. Flatten a list.
  * This isn't as good as other solutions, for example the following in hs:
  * data NestedList a = Elem a | List [NestedList a]
  * flatten :: NestedList a -> [a]
  * flatten (Elem a) = [a]
  * flatten (List (x:xs)) = flatten x ++ flatten (List xs)
  * flatten (List []) = []
  * which allows an arbitraily nested list of a specific type.
  *
  * The solution here works with List[Any], a very smelly type.
  */
  def flatten(xs: List[Any]): List[Any] = xs match {
    case Nil => Nil
    case (head: List[_]) :: tail => flatten(head) ++ flatten(tail)
    case head :: tail => head :: flatten(tail)
  }
  assert(flatten(List(1, 2, List(3, List(4, 5)))) == List(1, 2, 3, 4, 5))

  /**
  * 8. Remove consecutive duplicates.
  */
  def compress[T](items: List[T]): List[T] = items match {
    case Nil => Nil
    case x :: Nil                 => items
    case x :: y :: z if (x == y)  => x :: compress(z)
    case x :: y :: z              => x :: compress(y :: z)
  }
  assert(compress(List(1, 2, 2, 3)) == List(1, 2, 2, 3).distinct)

  /**
  * 9. Pack duplicate elements.
  */
  def pack[T](items: List[T]): List[List[T]] = {
    def help(items: List[T], acc: List[List[T]]): List[List[T]] = items match {
      case Nil => reverse(acc)
      case (x :: xs) if (acc.isEmpty) => help(xs, List(List(x)))
      case (x :: xs) => {
        if (x == acc.head.head) help(xs, (x :: acc.head) :: acc.tail)
        else help(xs, List(x) :: acc)
      }
    }
    help(items, List())
  }
  assert(pack(List(1, 2, 2, 3, 3, 3)) == List(1, 2, 2, 3, 3, 3)
                                        .groupBy(identity)
                                        .values
                                        .toList)

  /**
  * 10. Run-length encoding.
  */
  def encode[T](items: List[T]) = pack(items).map(x => (length(x), x.head))
  assert(encode(List(1, 2, 2, 3, 3, 3)) == List((1, 1), (2, 2), (3, 3)))


  /**
  * 11. Modified run-length encoding.
  */
  def modifiedEncode[T](items: List[T]): List[Any] =
    encode(items).map{case (length, elem) => if (length == 1) elem else (length, elem)}
  assert(modifiedEncode(List(1, 2, 2, 3, 3, 3)) == List(1, (2, 2), (3, 3)))


  /**
  * 12. Decode.
  */
  def decode[T](x: List[(Int, T)]): List[T] = x match {
    case Nil => Nil
    case (1, y) :: tail => y :: decode(tail)
    case (n, y) :: tail =>  y :: decode((n - 1, y) :: tail)
  }
  assert(decode(List((3, 'a'), (2, 'b'))) == List('a', 'a', 'a', 'b', 'b'))

  /**
  * 13. Encode 'from scratch'.
  */
  def encodeAgain[T](x: List[T]) = x.groupBy(identity).values.toList

  /**
  * 14. Duplicate elements.
  */
  def duplicate[T](x: List[T]): List[T] = x match {
    case Nil => Nil
    case x :: xs => List(x, x) ::: duplicate(xs)
  }
  assert(duplicate(List(1, 2, 3)) == List(1, 2, 3).flatMap(x => List(x, x)))


  /**
  * 15. Duplicate N times
  */
  def duplicateN[T](n: Int, x: List[T]): List[T] = {
    def repeat(n: Int, x: T): List[T] =
      if (n == 0) Nil else x :: repeat(n - 1, x)
    x match {
      case Nil => Nil
      case y :: ys => repeat(n, y) ::: duplicateN(n, ys)
    }
  }
  assert(duplicateN(3, List(1, 2)) == List(1, 1, 1, 2, 2, 2))


  /**
  * 16. Drop every nth element.
  */
  def drop[T](n: Int, x: List[T], offset: Int = 0): List[T] = x match {
    case Nil => Nil
    case x :: xs => if (offset == n - 1) drop(n, xs, 0) else x :: drop(n, xs, offset + 1)
  }
  assert(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))


  /**
  * 17. Split a list in two parts.
  */
  def split[T](n: Int, x: List[T]) = {
    def help(n: Int, x: List[T], acc: List[T]): (List[T], List[T]) =
      if (n == 0) (acc.reverse, x) else help(n - 1, x.tail, x.head :: acc)
    help(n, x, List())
  }
  val hello = "Hello!!".toList
  assert(split(2, hello) == (hello.take(2), hello.drop(2)))


  /**
  * 18. Extract a slice.
  */
  def slice[T](i: Int, j: Int, x: List[T]): List[T] = {
    def help(k: Int, x: List[T]): List[T] =
      if (k == 0) split(j - i, x)._1 else help(k - 1, x.tail)
    help(i, x)
  }
  assert(slice(2, 5, "hello there".toList) == "hello there".toList.slice(2, 5))

  /**
  * 19. Rotate n places to the left.
  */
  def rotate[T](n: Int, x: List[T]): List[T] =
    if (n == 0) x else rotate( n - 1, x.tail ::: List(x.head))
  assert(rotate(2, 1 :: 2 :: 3 :: Nil) == 3 :: 1 :: 2 :: Nil)

  /**
  * 20. Remove the i-th element.
  */
  def remove[T](i: Int, x: List[T]) = {
    val (l, r) = split(i + 1, x)
    (slice(0, length(l) - 1, l) ::: r, getAtIndex(i, x))
  }
  assert(remove(4, "abcdefgh".toList) == ("abcdfgh".toList, 'e'))

  /**
  * 21. Insert at index.
  */
  def insert[T](y: T, i: Int, x: List[T]) = {
    val (l, r) = split(i, x)
    l ::: List(y) ::: r
  }
  assert(insert('!', 2, "hello".toList) == "he!llo".toList)

  /*
   * 22. Create a range.
   */ 
  def range(i: Int, j: Int): List[Int] = 
    if (j == i + 1) List(i, j) else i :: range(i + 1, j)
  assert(range(2, 5) == List(2, 3, 4, 5))

  /* 
   * 23. Extract random selection. 
   */
  def randomSelect[T](n: Int, x: List[T]): List[T] = if (n == 0) Nil else {
      val i = util.Random.nextInt(length(x))
      val chosen = getAtIndex(i, x)
      val rest   = remove(i, x)
      chosen :: randomSelect(n - 1, rest._1)
    }
  
  /**
   * 24. N random numbers in 1...M.
   */
  def lotto(n: Int, m: Int) = randomSelect(n, range(1, m))

  /**
   * 25. Random permutation of list.
   */
  def shuffle[T](x: List[T]) = randomSelect(length(x), x)

  /**
   * 26. Generate all combinations of n elements in x.
   */
  def combinations[T](n: Int, x: List[T]): List[List[T]] = 
    if (n == 0) List(List[T]()) else (for {
    i <- 0 until x.size
    h = x(i)
    remainder = x drop i + 1
    rest <- combinations(n - 1, remainder)
  } yield h :: rest).toList
  assert("peach".toList.combinations(2).toList  == combinations(2, "peach".toList))

  /**
   * 27. Disjoint subsets. 
   * Generate all arrangments of disjoint subsets, given a list of subset sizes.
   */
  def group[T](sizes: List[Int], x: List[T]): List[List[List[T]]] = 
    if (sizes == Nil) List(List(List[T]()))  else (for {
      c    <- x.combinations(sizes.head)
      rest <- group(sizes.tail, x diff c)
    } yield c :: rest).toList
  // group(List(2, 3), "pineapl".toList) foreach println
  
  /**
   * 28. Sort a list of sublist according to length of sublist, and by 
   * sublength freq.
   */
  def lsort[T](x: List[List[T]]) = x.sortBy(_.size)
  def lsortFreq[T](x: List[List[T]]) = x.groupBy(_.size).toList.sortBy(_._2.size).map(_._2).flatten
}
