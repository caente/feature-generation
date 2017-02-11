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

object Main {

  implicit class FindOps[L <: HList](l: L) {
    def find[A](implicit f: Find[L, A]) = f.find(l)
  }

  @typeclass trait FeatureGenerator[X] {
    def features[C <: Product, L <: HList](x: X, c: C)(
      implicit
      gen: Generic.Aux[C, L],
      si: Find[L, Int],
      ss: Find[L, String],
      sd: Find[L, Double]
    ): Map[String, Int]
  }

  trait FindSubset[L <: HList, S <: HList] {
    def apply(l: L): Option[S]
  }

  object FindSubset {
    def apply[L <: HList, S <: HList](implicit f: FindSubset[L, S]) = f
    implicit def hcons[L <: HList, H, T <: HList](implicit find: Find[L, H], ft: FindSubset[L, T]) = new FindSubset[L, H :: T] {
      def apply(l: L) = (l.find[H] |@| FindSubset[L, T].apply(l)) {
        (h, t) => h :: t
      }
    }
    implicit def hnil[L <: HList] = new FindSubset[L, HNil] {
      def apply(l: L) = None
    }
  }

  implicit object StringAndContexts extends FeatureGenerator[String] {

    private def featureGenerator1(x: String): (Int, Double) => Map[String, Int] =
      (i, d) =>
        Map("ave" -> (d.toInt + i))

    private def featureGenerator2(x: String): (Int) => Map[String, Int] =
      i =>
        Map("dev" -> i)

    private def featureGenerator3(x: String): (String, Int) => Map[String, Int] =
      (s, i) =>
        Map("sum" -> (s.size + i + x.size))

    private def applyContext[L <: HList, Args <: HList, F](c: L)(f: F)(findSubset: FindSubset[L, Args])(
      implicit
      fp: FnToProduct.Aux[F, Args => Map[String, Int]]
    ): Map[String, Int] =
      findSubset(c).map {
        args =>
          f.toProduct(args)
      }.getOrElse(Map.empty)

    def features[C <: Product, L <: HList](x: String, c: C)(
      implicit
      gen: Generic.Aux[C, L],
      si: Find[L, Int],
      ss: Find[L, String],
      sd: Find[L, Double]
    ): Map[String, Int] = {
      val context = gen.to(c)
      applyContext(context)(featureGenerator1(x))(FindSubset[L, Int :: String :: HNil]) ++
        applyContext(context)(featureGenerator2(x))(FindSubset[L, Int :: HNil]) ++
        applyContext(context)(featureGenerator3(x))(FindSubset[L, Double :: Int :: HNil])
    }
  }

  def main(args: Array[String]): Unit = {
    import FeatureGenerator.ops._
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

