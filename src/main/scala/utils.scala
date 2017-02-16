package features
import shapeless._
import ops.hlist.Selector
import syntax.std.function._
import ops.function._
import simulacrum._
import scalaz._, Scalaz._

object utils {

  trait ApplyAll[Fs <: HList, Context <: HList] {
    type Out
    def apply(fs: Fs, context: Context): Seq[Out]
  }

  object ApplyAll {
    type Aux[Fs <: HList, Context <: HList, R] = ApplyAll[Fs, Context] { type Out = R }
    implicit def hcons[F, Fs <: HList, Context <: HList, Args <: HList, R](
      implicit
      fp: FnToProduct.Aux[F, Args => R],
      subset: Subset[Context, Args],
      applyContext: ApplyAll.Aux[Fs, Context, R]
    ): ApplyAll.Aux[F :: Fs, Context, R] = new ApplyAll[F :: Fs, Context] {
      type Out = R
      def apply(fs: F :: Fs, context: Context) =
        subset(context).map(args => fs.head.toProduct(args)).toSeq ++
          applyContext(fs.tail, context).toSeq
    }

    implicit def hnil[Context <: HList, R]: ApplyAll.Aux[HNil, Context, R] = new ApplyAll[HNil, Context] {
      type Out = R
      def apply(fs: HNil, context: Context) = Seq.empty
    }
  }

  def applyAll[Context <: HList, Fs <: HList, R](context: Context)(fs: Fs)(
    implicit
    applyContext: ApplyAll.Aux[Fs, Context, R]
  ): Seq[R] = applyContext(fs, context)

  def applyAll[Context <: Product, HContext <: HList, Fs <: HList, R](context: Context)(fs: Fs)(
    implicit
    gen: Generic.Aux[Context, HContext],
    applyContext: ApplyAll.Aux[Fs, HContext, R]
  ): Seq[R] = applyContext(fs, gen.to(context))

  def applyAll[X, Fs <: HList, R](x: X)(fs: Fs)(
    implicit
    applyContext: ApplyAll.Aux[Fs, X :: HNil, R]
  ): Seq[R] = applyContext(fs, x :: HNil)

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
