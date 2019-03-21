package functionals.laws

import functionals.MyId
import functionals.structures._
import functionals.typeclasses.{Equal, Functor}
import org.scalacheck.Arbitrary

import scala.language.{higherKinds, reflectiveCalls}

trait FunctorLawsSpec[F[_], A, B, C] { _: LawsSpec =>

  implicit def F: Functor[F]
  implicit def arbFA: Arbitrary[F[A]]
  implicit def arbAB: Arbitrary[A => B]
  implicit def arbBC: Arbitrary[B => C]
  implicit def eqFA: Equal[F[A]]
  implicit def eqFC: Equal[F[C]]

  def name: String

  lazy val laws: FunctorLaws[F] = FunctorLaws[F]

  property(s"A Functor[$name] should satisfy identity law") {
    forAll { fa: F[A] =>
      laws.functorIdentity(fa).isEqual shouldBe true
    }
  }

  property(s"A Functor[$name] should satisfy composition law") {
    forAll { (fa: F[A], f: A => B, g: B => C) =>
      laws.functorComposition(fa, f, g).isEqual shouldBe true
    }
  }
}

abstract class FunctorSpec[F[_], A, B, C](val name: String)(implicit val F: Functor[F],
                                                            val arbFA: Arbitrary[F[A]],
                                                            val arbAB: Arbitrary[A => B],
                                                            val arbBC: Arbitrary[B => C],
                                                            val eqFA: Equal[F[A]],
                                                            val eqFC: Equal[F[C]])
    extends LawsSpec with FunctorLawsSpec[F, A, B, C]

class MyIdFunctorSpec     extends FunctorSpec[MyId, Int, String, Long]("MyId")
class MyListFunctorSpec   extends FunctorSpec[MyList, Int, String, Long]("MyList")
class MyOptionFunctorSpec extends FunctorSpec[MyOption, Int, String, Long]("MyOption")
class MyEitherFunctorSpec extends FunctorSpec[({ type l[X] = MyEither[Int, X] })#l, Int, String, Long]("MyEither")
class MyTreeFunctorSpec   extends FunctorSpec[MyTree, Int, String, Long]("MyTree")
class MyValidatedFunctorSpec
    extends FunctorSpec[({ type l[X] = MyValidated[Int, X] })#l, Int, String, Long]("MyValidated")

class MyIdPureFunctorSpec
    extends FunctorSpec[MyId, Int, String, Long]("MyIdPure")(
      functionals.PureInstances.myIdFunctor,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )

class MyListPureFunctorSpec
    extends FunctorSpec[MyList, Int, String, Long]("MyListPure")(
      MyList.PureInstances.myListFunctor,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )

class MyOptionPureFunctorSpec
    extends FunctorSpec[MyOption, Int, String, Long]("MyOptionPure")(
      MyOption.PureInstances.myOptionFunctor,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )

class MyEitherPureFunctorSpec
    extends FunctorSpec[({ type l[X] = MyEither[Int, X] })#l, Int, String, Long]("MyEitherPure")(
      MyEither.PureInstances.myEitherFunctor,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )

class MyTreePureFunctorSpec
    extends FunctorSpec[MyTree, Int, String, Long]("MyTreePure")(
      MyTree.PureInstances.myTreeFunctor,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )

class MyValidatedPureFunctorSpec
    extends FunctorSpec[({ type l[X] = MyValidated[Int, X] })#l, Int, String, Long]("MyValidatedPure")(
      MyValidated.PureInstances.myValidatedFunctor,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )
