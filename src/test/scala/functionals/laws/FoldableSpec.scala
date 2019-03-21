package functionals.laws

import functionals.MyId
import functionals.structures.{MyEither, MyList, MyOption, MyValidated}
import functionals.typeclasses.{Equal, Foldable, Monoid}
import org.scalacheck.Arbitrary

import scala.language.higherKinds

trait FoldableLawsSpec[F[_], A, B] { _: LawsSpec =>

  def name: String

  implicit def F: Foldable[F]
  implicit def MA: Monoid[A]
  implicit def MB: Monoid[B]

  implicit def arbFA: Arbitrary[F[A]]
  implicit def arbAB: Arbitrary[A => B]
  implicit def eqA: Equal[A]
  implicit def eqB: Equal[B]

  lazy val laws: FoldableLaws[F] = FoldableLaws[F]

  property(s"A Foldable[$name] should satisfy left consistency with foldMap law") {
    forAll { (fa: F[A], f: A => B) =>
      laws.foldableLeftConsistenceWithFoldMap(fa, f).isEqual shouldBe true
    }
  }

  property(s"A Foldable[$name] should satisfy right consistency with foldMap law") {
    forAll { (fa: F[A], f: A => B) =>
      laws.foldableRightConsistenceWithFoldMap(fa, f).isEqual shouldBe true
    }
  }

  property(s"A Foldable[$name] should satisfy foldRef law") {
    forAll { fa: F[A] =>
      laws.foldRef(fa).isEqual shouldBe true
    }
  }
}

abstract class FoldableSpec[F[_], A, B](val name: String)(implicit val F: Foldable[F],
                                                          val arbFA: Arbitrary[F[A]],
                                                          val arbAB: Arbitrary[A => B],
                                                          val MA: Monoid[A],
                                                          val MB: Monoid[B],
                                                          val eqA: Equal[A],
                                                          val eqB: Equal[B])
    extends LawsSpec with FoldableLawsSpec[F, A, B]

class MyIdFoldableSpec        extends FoldableSpec[MyId, Int, String]("MyId")
class MyOptionFoldableSpec    extends FoldableSpec[MyOption, Int, String]("MyOption")
class MyListFoldableSpec      extends FoldableSpec[MyList, Int, String]("MyList")
class MyEitherFoldableSpec    extends FoldableSpec[({ type l[X] = MyEither[Int, X] })#l, Int, String]("MyEither")
class MyValidatedFoldableSpec extends FoldableSpec[({ type l[X] = MyValidated[Int, X] })#l, Int, String]("MyValidated")

class MyIdPureFoldableSpec
    extends FoldableSpec[MyId, Int, String]("MyIdPure")(
      functionals.PureInstances.myIdFoldable,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )

class MyOptionPureFoldableSpec
    extends FoldableSpec[MyOption, Int, String]("MyOptionPure")(
      MyOption.PureInstances.myOptionFoldable,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )

class MyListPureFoldableSpec
    extends FoldableSpec[MyList, Int, String]("MyListPure")(
      MyList.PureInstances.myListFoldable,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )

class MyEitherPureFoldableSpec
    extends FoldableSpec[({ type l[X] = MyEither[Int, X] })#l, Int, String]("MyEitherPure")(
      MyEither.PureInstances.myEitherFoldable,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )

class MyValidatedPureFoldableSpec
    extends FoldableSpec[({ type l[X] = MyValidated[Int, X] })#l, Int, String]("MyValidatedPure")(
      MyValidated.PureInstances.myValidatedFoldable,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )
