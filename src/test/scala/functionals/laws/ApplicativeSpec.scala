package functionals.laws

import functionals.MyId
import functionals.structures._
import functionals.typeclasses.{Applicative, Equal}
import org.scalacheck.Arbitrary

import scala.language.higherKinds

trait ApplicativeLawsSpec[F[_], A, B, C] extends FunctorLawsSpec[F, A, B, C] { _: LawsSpec =>

  override implicit def F: Applicative[F]

  implicit def arbA: Arbitrary[A]
  implicit def arbFAB: Arbitrary[F[A => B]]
  implicit def eqFB: Equal[F[B]]

  override lazy val laws: ApplicativeLaws[F] = ApplicativeLaws[F]

  property(s"An Applicative[$name] should satisfy homomorphism law") {
    forAll { (a: A, f: A => B) =>
      laws.homomorphism(a, f).isEqual shouldBe true
    }
  }

  property(s"An Applicative[$name] should satisfy identity law") {
    forAll { fa: F[A] =>
      laws.identity(fa).isEqual shouldBe true
    }
  }

  property(s"An Applicative[$name] should satisfy interchange law") {
    forAll { (a: A, ff: F[A => B]) =>
      laws.interchange(a, ff).isEqual shouldBe true
    }
  }

  property(s"An Applicative[$name] should satisfy mapConsistence law") {
    forAll { (fa: F[A], f: A => B) =>
      laws.mapConsistence(fa, f).isEqual shouldBe true
    }
  }
}

abstract class ApplicativeSpec[F[_], A, B, C](val name: String)(implicit val F: Applicative[F],
                                                                val arbFA: Arbitrary[F[A]],
                                                                val arbAB: Arbitrary[A => B],
                                                                val arbBC: Arbitrary[B => C],
                                                                val arbA: Arbitrary[A],
                                                                val arbFAB: Arbitrary[F[A => B]],
                                                                val eqFA: Equal[F[A]],
                                                                val eqFB: Equal[F[B]],
                                                                val eqFC: Equal[F[C]])
    extends LawsSpec with ApplicativeLawsSpec[F, A, B, C]

class MyIdApplicativeSpec     extends ApplicativeSpec[MyId, Int, String, Long]("MyId")
class MyListApplicativeSpec   extends ApplicativeSpec[MyList, Int, String, Long]("MyList")
class MyOptionApplicativeSpec extends ApplicativeSpec[MyOption, Int, String, Long]("MyOption")
class MyTreeApplicativeSpec   extends ApplicativeSpec[MyTree, Int, String, Long]("MyTree")
class MyEitherApplicativeSpec
    extends ApplicativeSpec[({ type l[X] = MyEither[Int, X] })#l, Int, String, Long]("MyEither")
class MyValidatedApplicativeSpec
    extends ApplicativeSpec[({ type l[X] = MyValidated[Int, X] })#l, Int, String, Long]("MyValidated")

class MyIdPureApplicativeSpec
    extends ApplicativeSpec[MyId, Int, String, Long]("MyIdPure")(
      functionals.PureInstances.myIdApplicative,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )

class MyListPureApplicativeSpec
    extends ApplicativeSpec[MyList, Int, String, Long]("MyListPure")(
      MyList.PureInstances.myListApplicative,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )

class MyOptionPureApplicativeSpec
    extends ApplicativeSpec[MyOption, Int, String, Long]("MyOptionPure")(
      MyOption.PureInstances.myOptionApplicative,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )

class MyTreePureApplicativeSpec
    extends ApplicativeSpec[MyTree, Int, String, Long]("MyTreePure")(
      MyTree.PureInstances.myTreeApplicative,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )

class MyEitherPureApplicativeSpec
    extends ApplicativeSpec[({ type l[X] = MyEither[Int, X] })#l, Int, String, Long]("MyEitherPure")(
      MyEither.PureInstances.myEitherApplicative,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )

class MyValidatedPureApplicativeSpec
    extends ApplicativeSpec[({ type l[X] = MyValidated[Int, X] })#l, Int, String, Long]("MyValidatedPure")(
      MyValidated.PureInstances.myValidatedApplicative,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )
