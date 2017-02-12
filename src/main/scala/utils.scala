package features
import shapeless._
import ops.hlist.Selector
import syntax.std.function._
import ops.function._
import simulacrum._
import scalaz._, Scalaz._

object utils {

  def applyContext[L <: HList, Args <: HList, F, R](c: L)(f: F)(
    implicit
    fp: FnToProduct.Aux[F, Args => R],
    subset: Subset.Aux[L, Args],
    mr: Monoid[R]
  ): R =
    subset(c).map {
      args =>
        f.toProduct(args)
    }.getOrElse(mr.zero)

  trait Find[L <: HList, A] {
    def find(l: L): Option[A]
  }

  object Find {
    def apply[A, L <: HList](implicit f: Find[L, A]) = f
    implicit class Ops[L <: HList](l: L) {
      def find[A](implicit f: Find[L, A]) = f.find(l)
    }
    implicit def hconsFound[A, H, T <: HList](implicit ev: H =:= A) = new Find[H :: T, A] {
      def find(l: H :: T) = Some(l.head)
    }
    implicit def hconsNotFound[A, H, T <: HList](implicit f: Find[T, A]) = new Find[H :: T, A] {
      def find(l: H :: T) = f.find(l.tail)
    }
    implicit def hnil[A] = new Find[HNil, A] {
      def find(l: HNil) = None
    }
  }
  import Find.Ops

  trait Subset[L <: HList] {
    type Out <: HList
    def apply(l: L): Option[Out]
  }

  object Subset {
    type Aux[L <: HList, S <: HList] = Subset[L] { type Out = S }
    def apply[L <: HList, S <: HList](implicit f: Subset.Aux[L, S]) = f

    implicit def hcons[L <: HList, H, T <: HList](
      implicit
      find: Find[L, H],
      subset: Lazy[Subset.Aux[L, T]]
    ) = new Subset[L] {
      type Out = H :: T
      def apply(l: L) =
        (l.find[H] |@| subset.value(l)) {
          (h, t) => h :: t
        }
    }

    implicit def hnil[L <: HList]: Subset.Aux[L, HNil] = new Subset[L] {
      type Out = HNil
      def apply(l: L) = Some(HNil)
    }
  }
}
