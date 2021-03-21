package monocleDemos

import monocle.Lens

object Lenses extends App { def f: Any = {

  case class Cat(name: String, age: Int)

  /** Lens[Cat, String] takes two args: 
   *  @param get: Cat => String
   *  @param set: String => Cat => Cat
   */
  val nameLens = Lens[Cat, String](_.name)(newName => cat => cat.copy(name = newName))

  // This pattern is repetetive so there is a macro:
  import monocle.macros.GenLens
  val nameLens2: Lens[Cat, String] = GenLens[Cat](_.name)

  // Using get and set:
  val snowy = Cat("Snowy", 2)
  nameLens.get(snowy)
  nameLens.set("Frosty")(snowy)

  // There is a convenient function if for Functor valued transformations:
  nameLens.modify(_ + " :)")(snowy)
  nameLens.set(nameLens.get(snowy) + " :)")(snowy)

  // There is also a pretty fancy function modifyF if the target is a functor:
  import cats.implicits._
  import cats.Functor

  implicitly(Functor[List])
  implicitly(Functor[Option])

  def upDown(n: Int): List[Int] = 
    if (n > 0) List(n - 1, n + 1) else List(n + 1)
  val x: List[Cat] = GenLens[Cat](_.age).modifyF(upDown)(snowy)
  
  /** If you want to insist that a cat have a name (i.e. the name field cannot 
   *  be the empty string) then rather than adding a require(name != "") into 
   *  the constructor, you could do 
   */
  def optName(s: String): Option[String] = if (s.size > 0) Some(s) else None
  val catWithName = nameLens.modifyF(optName)(snowy)
  val noCat       = nameLens.modifyF(optName)(Cat("", 42))

  // Perhaps especially useful for futures:
  import concurrent._
  import concurrent.ExecutionContext.Implicits._
  import concurrent.duration._
  
  def slowAPINewName(oldName: String): Future[String] = Future.successful("Frosty")
  val newCat = nameLens.modifyF(slowAPINewName)(snowy)
  Await.ready(newCat, 1.second)

  // Lenses can be composed:
  case class Family(paterfamilias: Cat, tabby: Boolean)
  val fam = Family(snowy, false)
  val catLens = GenLens[Family](_.paterfamilias)
  def greetTheBoss(f: Family): String = 
    "Hiiii " + (catLens composeLens nameLens).get(f)
  greetTheBoss(fam)

  
  




  }









println(f) }
