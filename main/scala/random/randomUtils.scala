package random

object RandomUtils {


  // Choose an element at random from an indexed seq
  def atRandom[T](xs: IndexedSeq[T]) =
    xs( util.Random.nextInt(xs.size) )

  // Choose a random element with weights
  type Weight = Int
  def atRandom[Elem](xs: Map[Elem, Weight]) = ???



}
