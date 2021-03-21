package monocleDemos

import cats.Eq
import cats.laws.discipline.FunctorTests
import monocle.law.discipline.LensTests
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.ScalacheckShapeless._
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import LensLaws.{ Cat, nameLens }


/** The imports are a bit but it's worth it to be able to check that a 
 *  lens (or other optic) is lawful with a single function call.
 */
class LensLawsTests extends AnyFunSuite 
  with FunSuiteDiscipline 
  with Checkers {
  
  implicit val catEq: Eq[Cat] = Eq.fromUniversalEquals

  checkAll("apply Cat lens", LensTests(nameLens)) 

}
