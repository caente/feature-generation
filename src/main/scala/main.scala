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
  import utils.applyContext

  trait FeatureGenerator[X] {
    type Context
    def features(x: X, context: Context): Map[String, Int]
  }

  object FeatureGenerator {

    implicit class Ops[X](x: X) {
      def features[C <: Product, L <: HList](context: C)(
        implicit
        gen: Generic.Aux[C, L],
        fg: FeatureGenerator[X] { type Context = L }
      ) =
        fg.features(x, gen.to(context))
    }
  }

  // features for String, each feature generator can process several contexts
  // the implicit `Find`s are for the types it _might_ need in its internal generators
  implicit def StringFeatures[L <: HList, F](
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
      applyContext(context)(featureGenerator1(x)) ++
        applyContext(context)(featureGenerator2(x)) ++
        applyContext(context)(featureGenerator3(x))
    }
  }

  def main(args: Array[String]): Unit = {
    import FeatureGenerator.Ops
    // it works with tuples and case classes
    assert(
      "hi".features(1, 2d) == Map("ave" -> 3, "dev" -> 1)
    )
    assert(
      "hi".features("a", 1) == Map("sum" -> 4, "dev" -> 1)
    )
    case class OneInt(i: Int)
    assert(
      "hi".features(OneInt(1)) == Map("dev" -> 1)
    )
    // the order of the arguments doesn't matter
    assert(
      "hi".features(2d, 1, "a") == Map("sum" -> 4, "dev" -> 1, "ave" -> 3)
    )

  }
}

