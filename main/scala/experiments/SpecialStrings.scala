package experiments

import eu.timepit.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.api.Refined

/** You want a string, but only if it matches a particular pattern.
 */
object SpecialStrings {
    
  type Animal = String Refined MatchesRegex["A|R|E|M"]

  case class Token(animal: Animal)

  // def f(t: Token): Token = t match {
  //   case Token("M") => Token("E")
  // }

}
