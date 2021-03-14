package shapeless

object Sandbox {
  

  /** Given types T1, ..., Tn, each of which is a semigroup, the product type 
   *  becomes a semigroup if the combine operation is defined elementwise.
   *
   *  Edit: Holy heck this looks a lot nicer in scala3/shapeless3.
   */

  /** First a simpler example. The Weighbridge typeclass has a unary function, 
   *  easier to reason with than a semigroup's binary one.
   */

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
    implicit val intWeighbridge    = instance[Int](x => x)
    implicit val stringWeighbridge = instance[String](x => x.size) 
    // inclucing a generic one for a tuple of two types for which instances
    // exist in scope.
    implicit def pairEncoder[S, T](implicit sWb: Weighbridge[S], tWb: Weighbridge[T]) = new Weighbridge[(S, T)] {
      def weight(x: (S, T)) = {
        val (s, t) = x
        sWb.weight(s) + tWb.weight(t)
      }
    }
  }


  // An user defined instance for a custom type;
  case class Cat(name: String, age: Int)
  implicit val catWeighbridge = Weighbridge.instance[Cat] { 
    case Cat(name, age) =>  Weighbridge[String].weight(name) + 
                            Weighbridge[Int].weight(age)
  }



  /* The problem is that we need to write a new method with this pattern for 
   * any length tuple. An even larger problem is that if we want to represent 
   * an algebraic data type using a case class (which we almost always do)
   * rather than a tuple we are nowhere; the compiler will not try to resolve
   * implicits for case class members. The solution is to transform the case 
   * class to an HList, work with that, then transform back to a our case class.
   */


}
