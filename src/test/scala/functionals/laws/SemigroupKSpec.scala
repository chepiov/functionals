package functionals.laws

import functionals.structures.{MyEither, MyList, MyOption, MyValidated}
import functionals.typeclasses.{Equal, SemigroupK}
import org.scalacheck.Arbitrary

import scala.language.higherKinds

trait SemigroupKLawsSpec[F[_], A] { _: LawsSpec =>

  implicit def F: SemigroupK[F]
  implicit def arbFA: Arbitrary[F[A]]
  implicit def eqFA: Equal[F[A]]

  def name: String

  lazy val laws: SemigroupKLaws[F] = SemigroupKLaws[F]

  property(s"A SemigroupK[$name] should satisfy combine associativity law") {
    forAll { (x: F[A], y: F[A], z: F[A]) =>
      laws.combineAssociativity(x, y, z).isEqual shouldBe true
    }
  }
}

abstract class SemigroupKSpec[F[_], A](val name: String)(implicit val F: SemigroupK[F],
                                                         val arbFA: Arbitrary[F[A]],
                                                         val eqFA: Equal[F[A]])
    extends LawsSpec with SemigroupKLawsSpec[F, A]

class MyListSemigroupKSpec      extends SemigroupKSpec[MyList, Int]("MyList")
class MyOptionSemigroupKSpec    extends SemigroupKSpec[MyOption, Int]("MyOption")
class MyEitherSemigroupKSpec    extends SemigroupKSpec[({ type l[X] = MyEither[Int, X] })#l, Int]("MyEither")
class MyValidatedSemigroupKSpec extends SemigroupKSpec[({ type l[X] = MyValidated[Int, X] })#l, Int]("MyValidated")

class MyListPureSemigroupKSpec
    extends SemigroupKSpec[MyList, Int]("MyListPure")(
      MyList.PureInstances.myListSemigroupK,
      implicitly,
      implicitly
    )

class MyOptionPureSemigroupKSpec
    extends SemigroupKSpec[MyOption, Int]("MyOptionPure")(
      MyOption.PureInstances.myOptionSemigroupK,
      implicitly,
      implicitly
    )

class MyEitherPureSemigroupKSpec
    extends SemigroupKSpec[({ type l[X] = MyEither[Int, X] })#l, Int]("MyEitherPure")(
      MyEither.PureInstances.semigroupK,
      implicitly,
      implicitly
    )

class MyValidatedPureSemigroupKSpec
    extends SemigroupKSpec[({ type l[X] = MyValidated[Int, X] })#l, Int]("MyValidatedPure")(
      MyValidated.PureInstances.myValidatedSemigroupK,
      implicitly,
      implicitly
    )
