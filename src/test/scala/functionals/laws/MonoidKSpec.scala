package functionals.laws

import functionals.structures.{MyList, MyOption}
import functionals.typeclasses.{Equal, MonoidK}
import org.scalacheck.Arbitrary

import scala.language.higherKinds

trait MonoidKLawSpec[F[_], A] extends SemigroupKLawsSpec[F, A] { _: LawsSpec =>

  implicit val F: MonoidK[F]

  def name: String

  override lazy val laws: MonoidKLaws[F] = MonoidKLaws[F]

  property(s"A MonoidK[$name] should satisfy combine right identity law") {
    forAll { fa: F[A] =>
      laws.combineRightIdentity(fa).isEqual shouldBe true
    }
  }

  property(s"A MonoidK[$name] should satisfy combine left identity law") {
    forAll { fa: F[A] =>
      laws.combineLeftIdentity(fa).isEqual shouldBe true
    }
  }
}

abstract class MonoidKSpec[F[_], A](val name: String)(implicit val F: MonoidK[F],
                                                      val arbFA: Arbitrary[F[A]],
                                                      val eqFA: Equal[F[A]])
    extends LawsSpec with MonoidKLawSpec[F, A]

class MyListMonoidKSpec   extends MonoidKSpec[MyList, Int]("MyList")
class MyOptionMonoidKSpec extends MonoidKSpec[MyOption, Int]("MyOption")

class MyListPureMonoidKSpec
    extends MonoidKSpec[MyList, Int]("MyListPure")(
      MyList.PureInstances.myListMonoidK,
      implicitly,
      implicitly
    )
class MyOptionPureMonoidKSpec
    extends MonoidKSpec[MyOption, Int]("MyOptionPure")(
      MyOption.PureInstances.myOptionMonoidK,
      implicitly,
      implicitly
    )
