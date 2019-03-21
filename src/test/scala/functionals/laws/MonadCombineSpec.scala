package functionals.laws

import functionals.structures.{MyList, MyOption}
import functionals.typeclasses.{Equal, MonadCombine}
import org.scalacheck.Arbitrary

import scala.language.higherKinds

trait MonadCombineLawsSpec[F[_], A, B, C] extends MonadFilterLawsSpec[F, A, B, C] with MonoidKLawSpec[F, A] {
  _: LawsSpec =>

  implicit val F: MonadCombine[F]

  override lazy val laws: MonadCombineLaws[F] = MonadCombineLaws[F]

  property(s"A MonadCombine[$name] should satisfy monadCombineLeftDistributivity law") {
    forAll { (fa: F[A], fa2: F[A], f: A => F[B]) =>
      laws.monadCombineLeftDistributivity(fa, fa2, f).isEqual shouldBe true
    }
  }
}

abstract class MonadCombineSpec[F[_], A, B, C](val name: String)(implicit val F: MonadCombine[F],
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
    extends LawsSpec with MonadCombineLawsSpec[F, A, B, C] {}

class MyListMonadCombineSpec   extends MonadCombineSpec[MyList, Int, String, Long]("MyList")
class MyOptionMonadCombineSpec extends MonadCombineSpec[MyOption, Int, String, Long]("MyOption")

class MyListPureMonadCombineSpec
    extends MonadCombineSpec[MyList, Int, String, Long]("MyListPure")(
      MyList.PureInstances.myListMonadCombine,
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

class MyOptionPureMonadCombineSpec
    extends MonadCombineSpec[MyOption, Int, String, Int]("MyOptionPure")(
      MyOption.PureInstances.myOptionMonadCombine,
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
