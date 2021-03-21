package monocleDemos 

import monocle.Lens
import monocle.macros.GenLens

/** See /tests/monocleDemos/LensLaws.scala for property checking of lens. 
 */
object LensLaws {
  case class Cat(name: String, age: Int)
  val nameLens: Lens[Cat, String] = GenLens[Cat](_.name)
}
