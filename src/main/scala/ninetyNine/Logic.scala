package ninetyNine 

object Logic extends App {

  // 46.
  def and(a: Boolean, b: Boolean) = 
    if (a == false) false else if (b == false) false else true
  def or(a: Boolean, b: Boolean) = 
    if (a == true) true else if (b == true) true else false
  def nand(a: Boolean, b: Boolean) = 
    if (a == false) true else if (b == false) true else false  
  def nor(a: Boolean, b: Boolean) = 
    if (a == true) false else if (b == true) false else true
  def xor(a: Boolean, b: Boolean) = 
    if (a == b) false else true
  def impl(a: Boolean, b: Boolean) = 
    if (and(a == true, b == false)) false else true
  def equ(a: Boolean, b: Boolean) = 
    a == b

  def table2(p: (Boolean, Boolean) => Boolean) = {
    val values = true :: false :: Nil
    println("A  B   result")
    for {
      a <- values
      b <- values
    } println(s"$a  $b  ${p(a, b)}")
  }

  // 49. Gray codes.  
  def gray(n: Int): List[List[Boolean]] = 
    if (n == 1) List(List(false), List(true)) else {
      val old = gray(n - 1)
      old.map(false :: _) ::: old.reverse.map(true :: _)
  }
  
  /**
   * 50. Huffman encoding.
   * Make a binary tree, define some methods on trees, define some helpers
   * on strings+lists of trees, define main method.
   */
  trait Tree
  case class Fork(l: Tree, r: Tree, chars: List[Char], wieght: Int) extends Tree
  case class Node(char: Char, weight: Int) extends Tree

  def weight(t: Tree) = t match {
    case Fork(_, _, _, w) => w
    case Node(_, w) => w
  }
  def chars(t: Tree) = t match {
    case Fork(_, _, c, _) => c
    case Node(c, _) => List(c)
  }
  def join(l: Tree, r: Tree) = 
    Fork(l, r, chars(l) ::: chars(r), weight(l) + weight(r))
 
  def frequencyTable(s: String) = s.toList.groupBy(identity).mapValues(_.size).toList.sortBy(_._2)
  def combine(t: List[Tree]) = if (t.size < 2) t else {
    val f = Fork(t(0), t(1), chars(t(0)) ++ chars(t(1)), weight(t(0)) + weight(t(1)))
    (f :: t.drop(2)).sortBy(weight)
  }
  
  def encode(s: String) = {
    val freqs = frequencyTable(s)
    val queue = freqs map { case (char, freq) => Node(char, freq) }
    def until[T](p: List[T] => Boolean, reduce: List[T] => List[T])(ts: List[T]): List[T] = {
      if (p(ts)) ts else until(p, reduce)(reduce(ts))
    }
    until((q: List[Tree]) => q.size == 1, combine)(queue)
  }

}
