import org.scalacheck._

object ListSpecifications extends Properties("List") {
  import Prop.forAll
  val propReverseList = forAll { l: List[String] => l.reverse.reverse == l }

  // val getAtIndexEmpty = forAll { i: Int =>
  //   assertThrows[Exception](Lists.getAtIndex(i, List()))
  // }
}
