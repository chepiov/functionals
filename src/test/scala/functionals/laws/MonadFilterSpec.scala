package functionals.laws

import functionals.structures.{MyList, MyOption}
import functionals.typeclasses.{Equal, MonadFilter}
import org.scalacheck.Arbitrary

import scala.language.higherKinds

//noinspection ScalaUnnecessaryParentheses
trait MonadFilterLawsSpec[F[_], A, B, C] extends MonadLawsSpec[F, A, B, C] { _: LawsSpec =>

  override implicit def F: MonadFilter[F]

  override lazy val laws: MonadFilterLaws[F] = MonadFilterLaws[F]

  property(s"A MonadFilter[$name] should satisfy filterLeftDistributivity law") {
    forAll { (f: A => F[B]) =>
      laws.filterLeftDistributivity(f).isEqual shouldBe true
    }
  }

  property(s"A MonadFilter[$name] should satisfy filterRightDistributivity law") {
    forAll { fa: F[A] =>
      laws.filterRightDistributivity(fa).isEqual shouldBe true
    }
  }
}

abstract class MonadFilterSpec[F[_], A, B, C](val name: String)(implicit val F: MonadFilter[F],
                                                                val arbFA: Arbitrary[F[A]],
                                                                val arbAB: Arbitrary[A => B],
                                                                val arbBC: Arbitrary[B => C],
                                                                val arbA: Arbitrary[A],
                                                                val arbFAB: Arbitrary[F[A => B]],
                                                                val arbAFB: Arbitrary[A => F[B]],
                                                                val arbBFC: Arbitrary[B => F[C]],
                                                                val eqFA: Equal[F[A]],
                                                                val eqFB: Equal[F[B]],
                                                                val eqFC: Equal[F[C]])
    extends LawsSpec with MonadFilterLawsSpec[F, A, B, C]

class MyListMonadFilterSpec   extends MonadFilterSpec[MyList, Int, String, Long]("MyList")
class MyOptionMonadFilterSpec extends MonadFilterSpec[MyOption, Int, String, Long]("MyOption")

class MyListPureMonadFilterSpec
    extends MonadFilterSpec[MyList, Int, String, Long]("MyListPure")(
      MyList.PureInstances.myListMonadFilter,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )

class MyOptionPureMonadFilterSpec
    extends MonadFilterSpec[MyOption, Int, String, Long]("MyOptionPure")(
      MyOption.PureInstances.myOptionMonadFilter,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )
