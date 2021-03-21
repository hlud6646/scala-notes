package ninetyNine

import org.scalacheck._
import org.scalacheck.Prop.forAll

object ListSpecifications extends Properties("List") {
  property("ReverseIsSelfInverse") = 
    forAll { l: List[String] => l.reverse.reverse == l }
}
