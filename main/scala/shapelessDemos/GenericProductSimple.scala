package shapelessDemos

import shapeless.{HList, ::, HNil, Generic}

/** Given some typeclass (TC) and instances for some types T1, ..., TN,
  *  it would be nice to have an instance for the product type for free.
  *  This demo uses a type class Weighbridge where a Weighbridge[T] is a
  *  way to ascribe an integer (weight?) to things of type T. The weight of
  *  a product is the sum of the weight of its parts.  This last part is
  *  not the only possible option, and so we expect that coding that part
  *  of the logic is unavoidable.
  */
object GenericProductSimple {

  // A custom typeclass.
  trait Weighbridge[T] {
    def weight(x: T): Int
  }

  // Idiomatic way to carry things around.
  object Weighbridge {
    // summoner;
    def apply[T](implicit wb: Weighbridge[T]) = wb
    // constructor;
    def instance[T](f: T => Int) = new Weighbridge[T] {
      def weight(x: T) = f(x)
    }
    // some globally visible instances which come shipped with this TC;
    implicit val intWeighbridge = instance[Int] { x => x }
    implicit val stringWeighbridge = instance[String] { x => x.size }
    // inclucing a generic one for a tuple of two types for which instances exist in scope.
    implicit def pairEncoder[S, T](implicit
        sWb: Weighbridge[S],
        tWb: Weighbridge[T]
    ): Weighbridge[(S, T)] =
      instance { case (s, t) => sWb.weight(s) + tWb.weight(t) }
  }

  // An user defined instance for a custom type (not making it implicit because we're defining a better one below)
  case class Cat(name: String, age: Int)
  val catWeighbridge = Weighbridge.instance[Cat] { case Cat(name, age) =>
    Weighbridge[String].weight(name) +
      Weighbridge[Int].weight(age)
  }

  /* The problem is that we need to write a new method with this pattern for
   * any length tuple. Worse, if we want to represent an algebraic data type
   * using a case class (which we almost always do) rather than a tuple we
   * are nowhere; the compiler will not try to resolve implicits for case class
   * members. The solution is to transform the case class to an HList, work
   * with that, then transform back to a our case class.
   */

  // Define instances for hlists.
  implicit val hnilWeighbridge: Weighbridge[HNil] = Weighbridge.instance { _ =>
    0
  }
  implicit def hlistWeighbridge[H, T <: HList](implicit
      hWb: Weighbridge[H],
      tWb: Weighbridge[T]
  ): Weighbridge[H :: T] =
    Weighbridge.instance { case h :: t =>
      hWb.weight(h) + tWb.weight(t)
    }

  // Our product type Cat is basically String |x| Int, so we can define an instance for that:
  val reprWeighbridge: Weighbridge[String :: Int :: HNil] = implicitly

  // and do
  reprWeighbridge.weight("Chairman Meow" :: 100 :: HNil)

  // and wrap this up to work for a Cat instance directly
  implicit val catWeighbridge2: Weighbridge[Cat] = {
    // an HList representation of the cat ADT.
    val gen = Generic[Cat]
    val wb = Weighbridge[gen.Repr]
    Weighbridge.instance { cat => wb.weight(gen.to(cat)) }
  }

  // and do
  Weighbridge[Cat].weight(Cat("Meow", 100))

  /* As far as this Cat datatype is concerned, we are done. Our existing instances for String, Int and HList
   * are enough to allow us to define one for Cat. But we still had to do that by hand, and so we havn't really
   * improved on the first effort where we encoded catWeighbridge directly.
   *
   * In the following definition, A is the product type for which we want an instance and R is the representation
   * of A as a type for which can create an instance.
   */
  implicit def genericProductWeighbridge[A, R](implicit
      gen: Generic[A] { type Repr = R },
      wb: Weighbridge[R]
  ): Weighbridge[A] = Weighbridge.instance { a =>
    wb.weight(gen.to(a))
  }

  // We can now auto derive an instance for any case class:
  case class Dog(age: Int, name: String)
  Weighbridge[Dog].weight(Dog(42, "Woof"))

  // A complete implementation without diversions might look like this:
  trait Length[T] {
    def length(x: T): Int
  }
  object Length {
    def apply[T](implicit l: Length[T]) = l
    def instance[T](f: T => Int) = new Length[T] { def length(x: T) = f(x) }
    implicit val hnilLength: Length[HNil] = instance { _ => 0 }
    implicit def hlistLength[H, T <: HList](implicit
        hLength: Length[H],
        tLength: Length[T]
    ): Length[H :: T] =
      instance { case h :: t => hLength.length(h) + tLength.length(t) }
    implicit def productLength[T, R](implicit
        gen: Generic[T] { type Repr = R },
        len: Length[R]
    ): Length[T] = instance { x => len.length(gen.to(x)) }
    implicit val intLength: Length[Int] = instance { x => if (x > 0) x else -x }
    implicit val boolLength: Length[Boolean] = instance { x => if (x) 1 else 0 }
    implicit val stringLength: Length[String] = instance { s => s.size }
  }
  case class Foop(a: Boolean, b: Int, c: Int, d: String)
  Length[Foop].length(Foop(true, 4, 2, "Woof"))

}
