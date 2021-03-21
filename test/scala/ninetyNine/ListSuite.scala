package ninetyNine
import org.scalatest.funsuite.AnyFunSuite

class ListSuite extends AnyFunSuite {

    test("Calling last on an empty list should raise Exception"){
      assertThrows[IllegalArgumentException] {
        Lists.last(List())
      }
    }

    test("Calling getAtIndex with index 0 on empty list should raise Exception"){
      assertThrows[Exception] {
        Lists.getAtIndex(0, List())
      }
    }
}
