package features
import shapeless._
import ops.hlist.Selector
import syntax.std.function._
import ops.function._
import simulacrum._
import scalaz._, Scalaz._

object Main {
  import utils.Find
  import utils.applyAll

  val featureGenerator1 = (x: String, i: Int, d: Double) => "feature1" -> (x + i.toString + d.toString)

  val featureGenerator2 = (x: String, i: Int) => "feature2" -> x.map(_.toInt).sum * i

  val featureGenerator3 = (x: String, s: Char, i: Int) => "feature3" -> (x.map(_.toInt).sum * 0.5 + i)

  val featureGenerator4 = (x: String) => ("string_size" -> x.size)

  object FeatureGenerators {

    val generators =
      featureGenerator1 ::
        featureGenerator2 ::
        featureGenerator3 ::
        featureGenerator4 :: HNil

    def features[Context <: Product, HContext <: HList](context: Context)(
      implicit
      gen: Generic.Aux[Context, HContext],
      distinct: IsDistinctConstraint[HContext],
      si: Find[HContext, Int],
      sc: Find[HContext, Char],
      ss: Find[HContext, String],
      sd: Find[HContext, Double]
    ) =
      applyAll(gen.to(context))(generators)

    def features[X](x: X)(
      implicit
      si: Find[X :: HNil, Int],
      sc: Find[X :: HNil, Char],
      ss: Find[X :: HNil, String],
      sd: Find[X :: HNil, Double]
    ) =
      applyAll(x :: HNil)(generators)

  }

  def main(args: Array[String]): Unit = {
    // it works with tuples and case classes
    val hi = "hi"
    assert(
      FeatureGenerators.features(hi) == "string_size" -> 2 :: HNil,
      FeatureGenerators.features(hi)
    )
    assert(
      FeatureGenerators.features(hi, 1) == "feature2" -> 209 :: "string_size" -> 2 :: HNil,
      FeatureGenerators.features(hi, 1)
    )
    assert(
      FeatureGenerators.features(hi, 1, 2d) == "feature1" -> "hi12.0" :: "feature2" -> 209 :: "string_size" -> 2 :: HNil,
      FeatureGenerators.features(hi, 1, 2d)
    )
    assert(
      FeatureGenerators.features(hi, 'a', 1) == "feature2" -> 209 :: "feature3" -> 105.5 :: "string_size" -> 2 :: HNil,
      FeatureGenerators.features(hi, 'a', 1)
    )
    // the order of the arguments doesn't matter
    assert(
      FeatureGenerators.features(hi, 2d, 1, 'a') == "feature1" -> "hi12.0" :: "feature2" -> 209 :: "feature3" -> 105.5 :: "string_size" -> 2 :: HNil,
      FeatureGenerators.features(hi, 2d, 1, 'a')
    )
  }
}

