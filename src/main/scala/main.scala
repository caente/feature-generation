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

  trait FeatureGenerator {
    def function: AnyRef
  }

  object featureGenerator1 extends FeatureGenerator {
    def function = (x: String, i: Int, d: Double) => ("ave" -> (d.toInt + i))
  }

  object featureGenerator2 extends FeatureGenerator {
    def function = (x: String, i: Int) => ("dev" -> i)
  }

  object featureGenerator3 extends FeatureGenerator {
    def function = (x: String, s: Char, i: Int) => ("sum" -> (s.toInt + i + x.size))
  }

  //is possible to have generators with the same context, they will all be used
  object featureGenerator3_1 extends FeatureGenerator {
    def function = (x: String, s: Char, i: Int) => ("sum2" -> (s.toInt + i * 2 + x.size))
  }

  object featureGenerator4 extends FeatureGenerator {
    def function = (x: String) => ("size" -> x.size)
  }

  object FeatureGenerators {

    val generators =
      featureGenerator1.function ::
        featureGenerator2.function ::
        featureGenerator3.function ::
        featureGenerator3_1.function ::
        featureGenerator4.function :: HNil

    def features[Context <: Product, HContext <: HList](context: Context)(
      implicit
      gen: Generic.Aux[Context, HContext],
      si: Find[HContext, Int],
      sc: Find[HContext, Char],
      ss: Find[HContext, String],
      sd: Find[HContext, Double]
    ): Seq[(String, Int)] =
      applyAll(gen.to(context))(generators)
  }

  def main(args: Array[String]): Unit = {
    // it works with tuples and case classes
    val hi = "hi"
    assert(
      FeatureGenerators.features(hi, 1) == Seq("dev" -> 1, "size" -> 2)
    )
    assert(
      FeatureGenerators.features(hi, 1, 2d) == Seq("ave" -> 3, "dev" -> 1, "size" -> 2)
    )
    assert(
      FeatureGenerators.features(hi, 'a', 1) == Seq("dev" -> 1, "sum" -> 100, "sum2" -> 101, "size" -> 2)
    )
    // the order of the arguments doesn't matter
    assert(
      FeatureGenerators.features(hi, 2d, 1, 'a') == Seq("ave" -> 3, "dev" -> 1, "sum" -> 100, "sum2" -> 101, "size" -> 2)
    )
  }
}

