import org.scalacheck.{ Gen, Properties, Arbitrary }
import org.scalacheck.Prop.forAll


import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline


object SringSpecs extends Properties("String") {
  
  property("String reverse is self inverse") = forAll { (a: String) => 
    a.reverse.reverse == a
  }

  property("joined string contain each substring") = forAll { (a: String, b: String) => 
    (a + b).contains(a) && (a + b).contains(b)
  }

}

// Bit of a strange place to be recording this but hey.
class DemoSuite extends AnyFunSuite 
  with FunSuiteDiscipline 
  with Checkers {

  // Integers in a range (inclusive)
  val smallInteger = Gen.choose(0, 100)

  /** Generator for random URL from 
   * https://medium.com/@supermanue/building-useful-scalacheck-generators-71635d1edb9d
   */
  def httpTypeGen   = Gen.oneOf("http" :: "https" :: Nil)
  def domainGen     = Gen.nonEmptyListOf(Gen.alphaNumStr).map(_.mkString("."))
  def domainTypeGen = Gen.oneOf("com" :: "org" :: "xzy" :: "gov" :: Nil)
  def pathGen       = Gen.nonEmptyListOf(Gen.alphaNumStr).map(_.mkString("/"))
  def urlGen: Gen[String] = for {
    httpType    <- httpTypeGen
    domain      <- domainGen
    domainType  <- domainTypeGen
    path        <- pathGen
  } yield httpType + "//" + domain + "." + domainType + "/" + path

  
  // Classic example.
  
  // Define your tree.
  sealed trait Tree[+A] {
    def size: Int
  }
  case object End extends Tree[Nothing] {
    def size = 0
  }
  case class Node[A](value: A, l: Tree[A], r: Tree[A]) extends Tree[A] {
    def size = 1 + l.size + r.size
  }

  // Define generators. 
  val genEnd = Gen.const(End)
  def genNode[A](implicit ev: Arbitrary[A]): Gen[Node[A]] = for {
    v <- Arbitrary.arbitrary[A]
    l <- genTree[A]
    r <- genTree[A]
  } yield Node(v, l, r)
  def genTree[A](implicit ev: Arbitrary[A]): Gen[Tree[A]] = 
    Gen.oneOf(genEnd, genNode)

  // An arbitrary tree in implicit scope.
  implicit def arbTree[A](implicit ev: Arbitrary[A]) = 
    Arbitrary(genTree[A])

  // Testing properties of trees.  Works, but where is the console output
  // acknowledging this was run?
  check(forAll{ t: Tree[Int] => 
    t.size >= 0
  })
  



}



  

