package features
import shapeless._
import ops.hlist.SelectAll
import syntax.std.function._
import ops.function._
import simulacrum._
import scalaz._, Scalaz._

object utils {

  def applyContext[Context <: HList, Args <: HList, F, R](context: Context)(f: F)(
    implicit
    fp: FnToProduct.Aux[F, Args => R],
    subset: PartialSelectAll[Context, Args],
    mr: Monoid[R]
  ): R =
    subset(context).map {
      args =>
        f.toProduct(args)
    }.getOrElse(mr.zero)

  trait PartialSelectAll[L <: HList, T] {
    def apply(l: L): Option[T]
  }

  object PartialSelectAll extends PartialSelectAll0 {
    implicit def present[L <: HList, T <: HList](
      implicit
      slt: SelectAll[L, T]
    ): PartialSelectAll[L, T] = new PartialSelectAll[L, T] {
      def apply(l: L): Option[T] = Some(slt(l))
    }
  }

  trait PartialSelectAll0 {
    implicit def absent[L <: HList, T]: PartialSelectAll[L, T] = new PartialSelectAll[L, T] {
      def apply(l: L): Option[T] = None
    }
  }

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

  trait Subset[L <: HList, S <: HList] {
    def apply(l: L): Option[S]
  }

  object Subset {
    def apply[L <: HList, S <: HList](implicit f: Subset[L, S]) = f

    implicit def hcons[L <: HList, H, T <: HList](
      implicit
      find: Find[L, H],
      subset: Lazy[Subset[L, T]]
    ) = new Subset[L, H :: T] {
      def apply(l: L) =
        (l.find[H] |@| subset.value(l)) {
          (h, t) => h :: t
        }
    }

    implicit def hnil[L <: HList]: Subset[L, HNil] = new Subset[L, HNil] {
      def apply(l: L) = Some(HNil)
    }
  }
}
