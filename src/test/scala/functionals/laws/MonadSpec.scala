package functionals.laws

import functionals.MyId
import functionals.structures._
import functionals.transformers.MyOptionT
import functionals.typeclasses.{Equal, Monad}
import org.scalacheck.Arbitrary

import scala.language.{higherKinds, reflectiveCalls}

trait MonadLawsSpec[F[_], A, B, C] extends ApplicativeLawsSpec[F, A, B, C] { _: LawsSpec =>

  override implicit def F: Monad[F]

  implicit def arbAFB: Arbitrary[A => F[B]]
  implicit def arbBFC: Arbitrary[B => F[C]]
  implicit def eqFA: Equal[F[A]]
  implicit def eqFB: Equal[F[B]]
  implicit def eqFC: Equal[F[C]]

  override lazy val laws: MonadLaws[F] = MonadLaws[F]

  property(s"A Monad[$name] should satisfy leftIdentity law") {
    forAll { (a: A, f: A => F[B]) =>
      laws.leftIdentity(a, f).isEqual shouldBe true
    }
  }

  property(s"A Monad[$name] should satisfy rightIdentity law") {
    forAll { fa: F[A] =>
      laws.rightIdentity(fa).isEqual shouldBe true
    }
  }

  property(s"A Monad[$name] should satisfy flatMapAssociativity law") {
    forAll { (fa: F[A], f: A => F[B], g: B => F[C]) =>
      laws.flatMapAssociativity(fa, f, g).isEqual shouldBe true
    }
  }
}

abstract class MonadSpec[F[_], A, B, C](val name: String)(implicit val F: Monad[F],
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
    extends LawsSpec with MonadLawsSpec[F, A, B, C]

class MyIdMonadSpec      extends MonadSpec[MyId, Int, String, Long]("MyId")
class MyOptionMonadSpec  extends MonadSpec[MyOption, Int, String, Long]("MyOption")
class MyListMonadSpec    extends MonadSpec[MyList, Int, String, Long]("MyList")
class MyEitherMonadSpec  extends MonadSpec[({ type l[X] = MyEither[Int, X] })#l, Int, String, Long]("MyEither")
class MyTreeMonadSpec    extends MonadSpec[MyTree, Int, String, Long]("MyTree")
class MyOptionTMonadSpec extends MonadSpec[({ type l[X] = MyOptionT[MyList, X] })#l, Int, String, Long]("MyOptionT")

class MyIdPureMonadSpec
    extends MonadSpec[MyId, Int, String, Long]("MyIdPure")(
      functionals.PureInstances.myIdMonad,
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

class MyListPureMonadSpec
    extends MonadSpec[MyList, Int, String, Long]("MyListPure")(
      MyList.PureInstances.myListMonad,
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

class MyOptionPureMonadSpec
    extends MonadSpec[MyOption, Int, String, Long]("MyOptionPure")(
      MyOption.PureInstances.myOptionMonad,
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

class MyTreePureMonadSpec
    extends MonadSpec[MyTree, Int, String, Long]("MyTreePure")(
      MyTree.PureInstances.myTreeMonad,
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

class MyEitherPureMonadSpec
    extends MonadSpec[({ type l[X] = MyEither[Int, X] })#l, Int, String, Long]("MyEitherPure")(
      MyEither.PureInstances.myEitherMonad,
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
