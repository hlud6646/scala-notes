package monocleDemos
/** Modifications with Monocle WILL NOT maintain explicitly passed paramters in 
 *  implicit parameter lists!
 */

import monocle.Focus
import monocle.macros.syntax.all._

trait Printer[A] {
  def style: String => String
}
object Printer {
  implicit def globalVersion[A] = new Printer[A] {
    def style = s => "- " + s + " -"
  }
}

object Main {

  case class Cat(name: String)(implicit p: Printer[Cat]){
    def show = println(p.style(name))
  }
  val sam = Cat("Sam")
  sam.show

  // Create a local printer
  val fancyPrinter = new Printer[Cat] {
    def style = s => "*** " + s + " ***"
  }

  // create a cat with this one
  val kerry = Cat("Kerry")(fancyPrinter)
  kerry.show

  // and modify it:
  val jerry = kerry.focus(_.name).replace("Jerry")
  jerry.show
}
