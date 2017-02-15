package features
import shapeless._
import ops.hlist.{ Selector, SelectAll }
import syntax.std.function._
import ops.function._
import simulacrum._
import scalaz._, Scalaz._

object utils {

  trait ApplyContext[Fs <: HList, Context <: HList] {
    type Out
    def apply(fs: Fs, context: Context): Out
  }

  trait ApplyContextLowPriority {
    implicit def hconsFound[F, Fs <: HList, Context <: HList, R](
      implicit
      applyContext: ApplyContext.Aux[Fs, Context, R],
      mr: Monoid[R]
    ): ApplyContext[F :: Fs, Context] = new ApplyContext[F :: Fs, Context] {
      type Out = R
      def apply(fs: F :: Fs, context: Context) = applyContext(fs.tail, context)
    }

    implicit def hconsFound[F, Fs <: HList, Context <: HList, R](
      implicit
      mr: Monoid[R]
    ): ApplyContext[F :: Fs, Context] = new ApplyContext[F :: Fs, Context] {
      type Out = R
      def apply(fs: F :: Fs, context: Context) = mr.zero
    }
  }
  object ApplyContext extends ApplyContextLowPriority {
    type Aux[Fs <: HList, Context <: HList, R] = ApplyContext[Fs, Context] { type Out = R }
    implicit def hconsFound[F, Fs <: HList, Context <: HList, Args <: HList, R](
      implicit
      fp: FnToProduct.Aux[F, Args => R],
      selectAll: SelectAll[Context, Args],
      applyContext: ApplyContext.Aux[Fs, Context, R],
      mr: Monoid[R]
    ): ApplyContext[F :: Fs, Context] = new ApplyContext[F :: Fs, Context] {
      type Out = R
      def apply(fs: F :: Fs, context: Context) =
        fs.head.toProduct(selectAll(context)) |+|
          applyContext(fs.tail, context)
    }

    implicit def hnil[Context <: HList, R](implicit mr: Monoid[R]) = new ApplyContext[HNil, Context] {
      type Out = R
      def apply(fs: HNil, context: Context) = mr.zero
    }
  }

  def applyContexts[Context <: HList, Args <: HList, Fs <: HList, R](context: Context)(fs: Fs)(
    implicit
    applyContext: ApplyContext.Aux[Fs, Context, R]
  ): R = applyContext(fs, context)

  def applyContext[Context <: HList, Args <: HList, F, R](context: Context)(f: F)(
    implicit
    fp: FnToProduct.Aux[F, Args => R],
    subset: Subset[Context, Args],
    mr: Monoid[R]
  ): R =
    subset(context).map {
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
