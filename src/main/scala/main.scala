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
  import utils.applyContexts

  trait GeneralFeature {
    def name: String = ??? // the name comes here
  }

  trait LabelessFeature extends GeneralFeature {
    def run: AnyRef
  }

  object featureGenerator1L extends LabelessFeature {
    def run = (i: Int) => Map("dev" -> i)
  }
  object featureGenerator2L extends LabelessFeature {
    def run = (s: String, i: Int) => Map("sum" -> (s.size + i))
  }

  object featureGenerator3L extends LabelessFeature {
    def run = () => Map("size" -> 0)
  }

  trait Feature[X] extends GeneralFeature {
    def run(x: X): AnyRef
  }

  object featureGenerator1 extends Feature[String] {
    def run(x: String) = (i: Int, d: Double) => Map("ave" -> (d.toInt + i))
  }

  object featureGenerator2 {
    def run(x: String) = (i: Int) => Map("dev" -> i)
  }

  object featureGenerator3 {
    def run(x: String) = (s: String, i: Int) => Map("sum" -> (s.size + i + x.size))
  }

  //is possible to have generators with the same context, they will all be used
  object featureGenerator3_1 {
    def run(x: String) = (s: String, i: Int) => Map("sum2" -> (s.size + i * 2 + x.size))
  }

  object featureGenerator4 {
    def run(x: String) = () => Map("size" -> (x.size))
  }

  trait FeatureGenerator[X] {
    type Context
    def features(x: X, context: Context): Map[String, Int]
  }

  // features for String, each feature generator can process several contexts
  // the implicit `Find`s are for the types it _might_ need in its internal generators
  implicit def StringFeatures[L <: HList, F](
    implicit
    si: Selector[L, Int],
    ss: Selector[L, String],
    sd: Selector[L, Double]
  ) = new FeatureGenerator[String] {
    type Context = L

    def features(x: String, context: Context): Map[String, Int] = {
      val fgs = featureGenerator1.run(x) ::
        featureGenerator2.run(x) ::
        featureGenerator3.run(x) ::
        featureGenerator3_1.run(x) ::
        featureGenerator4.run(x) :: HNil
      applyContexts(context)(fgs)
      //     applyContext(context)(featureGenerator1.run(x)) ++
      //       applyContext(context)(featureGenerator2.run(x)) ++
      //       applyContext(context)(featureGenerator3.run(x)) ++
      //       applyContext(context)(featureGenerator3_1.run(x)) ++
      //       applyContext(context)(featureGenerator4.run(x))
    }
  }

  implicit def LabelessFeatures[L <: HList, F](
    implicit
    si: Find[L, Int],
    ss: Find[L, String]
  ) = new FeatureGenerator[FeatureGenerator.Labeless.type] {
    type Context = L

    def features(x: FeatureGenerator.Labeless.type, context: Context): Map[String, Int] = {
      applyContext(context)(featureGenerator1L.run) ++
        applyContext(context)(featureGenerator2L.run) ++
        applyContext(context)(featureGenerator3L.run)
    }
  }

  object FeatureGenerator {

    implicit class Ops[X](x: X) {
      //instead of Product, we could do C <: BaseContext,
      //to be more strict regarding the context possible values
      def features[C <: Product, L <: HList](context: C)(
        implicit
        gen: Generic.Aux[C, L],
        fg: FeatureGenerator[X] { type Context = L }
      ) =
        fg.features(x, gen.to(context))
      def features[C](context: C)(
        implicit
        fg: FeatureGenerator[X] { type Context = C :: HNil }
      ) =
        fg.features(x, context :: HNil)
    }
    case object Labeless
    case class NoContext()
    def emptyContext: NoContext = NoContext()
  }

  def main(args: Array[String]): Unit = {
    import FeatureGenerator.Ops
    // it works with tuples and case classes
    assert(
      "hi".features(FeatureGenerator.emptyContext) === Map("size" -> 2)
    )
    assert(
      "hi".features(1) === Map("dev" -> 1, "size" -> 2)
    )
    assert(
      "hi".features(1, 2d) === Map("ave" -> 3, "dev" -> 1, "size" -> 2)
    )
    assert(
      "hi".features("a", 1) === Map("sum" -> 4, "sum2" -> 5, "dev" -> 1, "size" -> 2)
    )
    // the order of the arguments doesn't matter
    assert(
      "hi".features(2d, 1, "a") === Map("sum" -> 4, "sum2" -> 5, "dev" -> 1, "ave" -> 3, "size" -> 2)
    )

    // feature generators with no X
    assert(
      FeatureGenerator.Labeless.features(FeatureGenerator.emptyContext) === Map("size" -> 0)
    )
    assert(
      FeatureGenerator.Labeless.features(1) === Map("dev" -> 1, "size" -> 0)
    )
    assert(
      FeatureGenerator.Labeless.features("a", 1) === Map("sum" -> 2, "dev" -> 1, "size" -> 0)
    )
    // the order of the arguments doesn't matter
    assert(
      FeatureGenerator.Labeless.features(1, "a") === Map("sum" -> 2, "dev" -> 1, "size" -> 0)
    )

  }
}

