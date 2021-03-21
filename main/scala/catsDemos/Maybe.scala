package catsDemos

import cats.{ Eq, Functor }


/** In this demo, reimplelemt the optional type and make it a functor. 
 *  See tests.LawsTesting.scala for validation that the functor is lawful.
 */

sealed trait Maybe[+T]
case object Nope extends Maybe[Nothing]
case class Just[T](x: T) extends Maybe[T]

object Maybe {

  /** Something in the tests need an cats.Eq[Maybe] in scope.
   *  This one simply uses ==, which is fine for our case class.
   */
  implicit def eqMaybe[A: Eq]: Eq[Maybe[A]] = Eq.fromUniversalEquals

  // The functor whose lawfulness will be examined.
  implicit val mf = new Functor[Maybe] {
    def map[A, B](x: Maybe[A])(f: A => B): Maybe[B] = x match {
      case Nope => Nope
      case Just(x) => Just(f(x))
    }
  }
  
}
