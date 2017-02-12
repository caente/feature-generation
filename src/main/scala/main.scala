package features
import shapeless._
import ops.hlist.Selector
import syntax.std.function._
import ops.function._
import simulacrum._
import scalaz._, Scalaz._

object Main {
  import utils.Find
  import utils.Subset

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

  // features for String, each feature generator can process several contexts
  // the implicit `Find`s are for the types it _might_ need in its internal generators
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

    def features(x: String, context: Context): Map[String, Int] = {
      // this type annotations should go away soon, but I haven't figured out how yet, perhaps a macro?
      applyContext[L, Int :: Double :: HNil, Function2[Int, Double, Map[String, Int]]](context)(featureGenerator1(x)) ++
        applyContext[L, Int :: HNil, Function1[Int, Map[String, Int]]](context)(featureGenerator2(x)) ++
        applyContext[L, String :: Int :: HNil, Function2[String, Int, Map[String, Int]]](context)(featureGenerator3(x))
    }

    private def applyContext[L <: HList, Args <: HList, F](c: L)(f: F)(
      implicit
      subset: Lazy[Subset.Aux[L, Args]],
      fp: FnToProduct.Aux[F, Args => Map[String, Int]]
    ): Map[String, Int] =
      subset.value.subset(c).map {
        args =>
          f.toProduct(args)
      }.getOrElse(Map.empty)
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

