package features
import shapeless._
import ops.hlist.Selector
import syntax.std.function._
import ops.function._
import simulacrum._
import scalaz._, Scalaz._

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
    def subset(l: L) = {
      l match {
        case HNil => None
        case l => (l.find[H] |@| ft.value.subset(l)) {
          (h, t) => h :: t
        }
      }
    }
  }

  implicit def hnil[L <: HList]: Subset.Aux[L, HNil] = new Subset[L] {
    type Out = HNil
    def subset(l: L) = Some(HNil)
  }
}

object Main {

  trait FeatureGenerator[X] {
    type Context
    def features(x: X, context: Context): Map[String, Int]
  }

  object FeatureGenerator {
    implicit class Ops[X](x: X) {
      def features[C <: Product, L <: HList](context: C)(implicit gen: Generic.Aux[C, L], fg: FeatureGenerator[X] { type Context = L }) =
        fg.features(x, gen.to(context))
    }
  }

  implicit def StringFeatures[L <: HList](
    implicit
    si: Find[L, Int],
    ss: Find[L, String],
    sd: Find[L, Double]
  ) = new FeatureGenerator[String] {
    type Context = L

    private def featureGenerator1(x: String): (Int, Double) => Map[String, Int] =
      (i, d) =>
        Map("ave" -> (d.toInt + i))

    private def featureGenerator2(x: String): (Int) => Map[String, Int] =
      i =>
        Map("dev" -> i)

    private def featureGenerator3(x: String): (String, Int) => Map[String, Int] =
      (s, i) =>
        Map("sum" -> (s.size + i + x.size))

    private def applyContext[L <: HList, Args <: HList, F](c: L)(f: F)(
      implicit
      subset: Lazy[Subset.Aux[L, Args]],
      fp: FnToProduct.Aux[F, Args => Map[String, Int]]
    ): Map[String, Int] =
      subset.value.subset(c).map {
        args =>
          f.toProduct(args)
      }.getOrElse(Map.empty)

    def features(x: String, context: Context): Map[String, Int] = {
      // this type annoations should go away soon, but I haven't figured out how, yet
      applyContext[L, Int :: Double :: HNil, Function2[Int, Double, Map[String, Int]]](context)(featureGenerator1(x)) ++
        applyContext[L, Int :: HNil, Function1[Int, Map[String, Int]]](context)(featureGenerator2(x)) ++
        applyContext[L, String :: Int :: HNil, Function2[String, Int, Map[String, Int]]](context)(featureGenerator3(x))
    }
  }

  def main(args: Array[String]): Unit = {
    import FeatureGenerator.Ops
    // it works with tuples and case classes
    assert(
      "hi".features((1, 2d)) == Map("ave" -> 3, "dev" -> 1)
    )
    assert(
      "hi".features(("a", 1)) == Map("sum" -> 4, "dev" -> 1)
    )
    case class OneInt(i: Int)
    assert(
      "hi".features(OneInt(1)) == Map("dev" -> 1)
    )

  }
}

