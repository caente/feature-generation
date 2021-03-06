package features
import shapeless._
import ops.hlist.Selector
import syntax.std.function._
import ops.function._
import simulacrum._
import scalaz._, Scalaz._

object Main {
  import utils.Find
  import utils.ApplyAll

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

    def features[Context <: HList, Fs <: HList, R](context: Context)(fs: Fs)(
      implicit
      applyContext: ApplyAll.Aux[Fs, Context, R]
    ): Seq[R] = applyContext(fs, context)

    def features[Context <: Product, HContext <: HList, Fs <: HList, R](context: Context)(fs: Fs)(
      implicit
      gen: Generic.Aux[Context, HContext],
      applyContext: ApplyAll.Aux[Fs, HContext, R]
    ): Seq[R] = applyContext(fs, gen.to(context))

    def features[X, Fs <: HList, R](x: X)(fs: Fs)(
      implicit
      applyContext: ApplyAll.Aux[Fs, X :: HNil, R]
    ): Seq[R] = applyContext(fs, x :: HNil)

  }

  def main(args: Array[String]): Unit = {
    // it works with tuples and case classes
    val hi = "hi"
    assert(
      FeatureGenerators.features(hi)(FeatureGenerators.generators) == Seq("string_size" -> 2)
    )
    assert(
      FeatureGenerators.features(hi, 1)(FeatureGenerators.generators) == Seq("feature2" -> 1, "string_size" -> 2)
    )
    assert(
      FeatureGenerators.features(hi, 1, 2d)(FeatureGenerators.generators) == Seq("feature1" -> 3, "feature2" -> 1, "string_size" -> 2)
    )
    assert(
      FeatureGenerators.features(hi, 'a', 1)(FeatureGenerators.generators) == Seq("feature2" -> 1, "feature3" -> 100, "feature3_1" -> 101, "string_size" -> 2)
    )
    // the order of the arguments doesn't matter
    assert(
      FeatureGenerators.features(hi, 2d, 1, 'a')(FeatureGenerators.generators) == Seq("feature1" -> 3, "feature2" -> 1, "feature3" -> 100, "feature3_1" -> 101, "string_size" -> 2)
    )
  }
}

