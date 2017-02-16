package features
import shapeless._
import ops.hlist.Selector
import syntax.std.function._
import ops.function._
import simulacrum._
import scalaz._, Scalaz._

object utils {

  trait ApplyAll[Fs <: HList, Context <: HList] {
    def apply(fs: Fs, context: Context): HList
  }

  object ApplyAll {
    implicit def hcons[F, Fs <: HList, Context <: HList, Args <: HList, R](
      implicit
      fp: FnToProduct.Aux[F, Args => R],
      subset: Subset[Context, Args],
      applyContext: ApplyAll[Fs, Context]
    ): ApplyAll[F :: Fs, Context] = new ApplyAll[F :: Fs, Context] {
      def apply(fs: F :: Fs, context: Context) =
        subset(context).map(
          args =>
            fs.head.toProduct(args) :: applyContext(fs.tail, context)
        ).getOrElse(applyContext(fs.tail, context))
    }

    implicit def hnil[Context <: HList]: ApplyAll[HNil, Context] = new ApplyAll[HNil, Context] {
      def apply(fs: HNil, context: Context) = HNil
    }
  }

  def applyAll[Context <: HList, Fs <: HList](context: Context)(fs: Fs)(
    implicit
    applyContext: ApplyAll[Fs, Context]
  ) = applyContext(fs, context)

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
