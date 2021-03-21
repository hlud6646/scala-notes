package catsDemos

import cats.implicits._
import cats.laws.discipline.FunctorTests

// Your wild testing tools.
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

// Import this to get arbitaries of user defined types.
import org.scalacheck.ScalacheckShapeless._

class MaybeLawTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {

  checkAll("Maybe.FunctorLaws", FunctorTests[Maybe].functor[Int, Int, String])

}
