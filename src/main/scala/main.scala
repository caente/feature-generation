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

  val featureGenerator1 = (x: String, i: Int, d: Double) => ("feature1" -> (d.toInt + i))

  val featureGenerator2 = (x: String, i: Int) => ("feature2" -> i)

  val featureGenerator3 = (x: String, s: Char, i: Int) => ("feature3" -> (s.toInt + i + x.size))

  //is possible to have generators with the same context, they will all be used
  val featureGenerator3_1 = (x: String, s: Char, i: Int) => ("feature3_1" -> (s.toInt + i * 2 + x.size))

  val featureGenerator4 = (x: String) => ("string_size" -> x.size)

  object FeatureGenerators {

    val generators =
      featureGenerator1 ::
        featureGenerator2 ::
        featureGenerator3 ::
        featureGenerator3_1 ::
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
      FeatureGenerators.features(hi) == Seq("string_size" -> 2)
    )
    assert(
      FeatureGenerators.features(hi, 1) == Seq("feature2" -> 1, "string_size" -> 2)
    )
    assert(
      FeatureGenerators.features(hi, 1, 2d) == Seq("feature1" -> 3, "feature2" -> 1, "string_size" -> 2)
    )
    assert(
      FeatureGenerators.features(hi, 'a', 1) == Seq("feature2" -> 1, "feature3" -> 100, "feature3_1" -> 101, "string_size" -> 2)
    )
    // the order of the arguments doesn't matter
    assert(
      FeatureGenerators.features(hi, 2d, 1, 'a') == Seq("feature1" -> 3, "feature2" -> 1, "feature3" -> 100, "feature3_1" -> 101, "string_size" -> 2)
    )
  }
}

