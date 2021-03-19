package monocleDemos

import monocle.Focus
import monocle.macros.syntax.all._


/** A nested field contains a list, and you want to modify
 *  the first element in the list matching some predicate.
 *  When you think about it, identifying the problem data and
 *  then replacing can happily be kept wide apart.
 */
object modifyListElem{
  
  case class Foo(items: List[Int])
  val f = Foo(List(1, 2, 7, 4))
  val i = f.items indexOf 7
  f.focus(_.items).index(i).replace(3)
}
