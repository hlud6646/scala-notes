package shapelessDemos

import shapeless.{HNil, ::, HList, Generic}

/** Given semigroups T1, ..., TN one forms the product semigroup by defining the combine opertaion
  *  pointwise, that is (x1, ..., xn) |+| (y1, ..., yn) = (x1 |+| y1, ..., xn |+| yn).
  */
object GenericProductSemigroup {

  trait Semigroup[T] {
    def combine(x: T, y: T): T
  }

  object Semigroup {
    def apply[T](implicit sg: Semigroup[T]) = sg
    def instance[T](f: (T, T) => T) = new Semigroup[T] {
      def combine(x: T, y: T) = f(x, y)
    }
    implicit val intSemigroup: Semigroup[Int] = instance { (x, y) => x + y }
    implicit val stringSemigroup: Semigroup[String] = instance { (x, y) =>
      x + y
    }
    implicit val hnilSemigroup: Semigroup[HNil] = instance { (_, _) => HNil }
    implicit def hlistSemigroup[H, T <: HList](implicit
        hSg: Semigroup[H],
        tSg: Semigroup[T]
    ): Semigroup[H :: T] = instance { case (h1 :: t1, h2 :: t2) =>
      hSg.combine(h1, h2) :: tSg.combine(t1, t2)
    }
    implicit def productSemigroup[T, R](implicit
        gen: Generic[T] { type Repr = R },
        sg: Semigroup[R]
    ): Semigroup[T] = instance {
      case (x, y) => { gen.from(sg.combine(gen.to(x), gen.to(y))) }
    }
  }

  case class Foo(i: Int, s: String)

  val x = Foo(1, "123")
  val y = Foo(3, "456")

  Semigroup[Foo].combine(x, y)
}
