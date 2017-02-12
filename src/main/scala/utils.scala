package features
import shapeless._
import ops.hlist.Selector
import syntax.std.function._
import ops.function._
import simulacrum._
import scalaz._, Scalaz._

object utils {
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
    def subset(l: L): Option[Out]
  }

  object Subset {
    type Aux[L <: HList, S <: HList] = Subset[L] { type Out = S }
    def apply[L <: HList, S <: HList](implicit f: Subset.Aux[L, S]) = f

    implicit def hcons[L <: HList, H, T <: HList](implicit find: Find[L, H], ft: Lazy[Subset.Aux[L, T]]) = new Subset[L] {
      type Out = H :: T
      def subset(l: L) =
        (l.find[H] |@| ft.value.subset(l)) {
          (h, t) => h :: t
        }
    }

    implicit def hnil[L <: HList]: Subset.Aux[L, HNil] = new Subset[L] {
      type Out = HNil
      def subset(l: L) = Some(HNil)
    }
  }
}
