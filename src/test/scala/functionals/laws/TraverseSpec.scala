package functionals.laws

import functionals.MyId
import functionals.structures._
import functionals.typeclasses.{Equal, Monoid, Traverse}
import org.scalacheck.Arbitrary

import scala.language.{higherKinds, reflectiveCalls}

trait TraverseLawsSpec[F[_], A, B, C] extends FunctorLawsSpec[F, A, B, C] with FoldableLawsSpec[F, A, B] {
  _: LawsSpec =>

  override implicit def F: Traverse[F]

  implicit def arbA: Arbitrary[A]
  implicit def arbFAB: Arbitrary[F[A => B]]
  implicit def eqFB: Equal[F[B]]

  override lazy val laws: TraverseLaws[F] = TraverseLaws[F]

  property(s"A Traverse[$name] should satisfy identity law") {
    forAll { (fa: F[A], f: A => B) =>
      laws.traverseIdentity(fa, f).isEqual shouldBe true
    }
  }
}

abstract class TraverseSpec[F[_], A, B, C](val name: String)(implicit val F: Traverse[F],
                                                             val arbFA: Arbitrary[F[A]],
                                                             val arbAB: Arbitrary[A => B],
                                                             val arbBC: Arbitrary[B => C],
                                                             val arbA: Arbitrary[A],
                                                             val arbFAB: Arbitrary[F[A => B]],
                                                             val MA: Monoid[A],
                                                             val MB: Monoid[B],
                                                             val eqA: Equal[A],
                                                             val eqB: Equal[B],
                                                             val eqFA: Equal[F[A]],
                                                             val eqFB: Equal[F[B]],
                                                             val eqFC: Equal[F[C]])
    extends LawsSpec with TraverseLawsSpec[F, A, B, C]

class MyIdTraverseSpec     extends TraverseSpec[MyId, Int, String, Long]("MyId")
class MyOptionTraverseSpec extends TraverseSpec[MyOption, Int, String, Long]("MyOption")
class MyListTraverseSpec   extends TraverseSpec[MyList, Int, String, Long]("MyList")
class MyEitherTraverseSpec extends TraverseSpec[({ type l[X] = MyEither[Int, X] })#l, Int, String, Long]("MyEither")
class MyValidatedTraverseSpec
    extends TraverseSpec[({ type l[X] = MyValidated[Int, X] })#l, Int, String, Long]("MyValidated")
class MyTreeTraverseSpec extends TraverseSpec[MyTree, Int, String, Long]("MyTree")

class MyIdPureTraverseSpec
    extends TraverseSpec[MyId, Int, String, Long]("MyIdPure")(
      functionals.PureInstances.myIdTraverse,
      implicitly,
      implicitly,
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

class MyOptionPureTraverseSpec
    extends TraverseSpec[MyOption, Int, String, Long]("MyOptionPure")(
      MyOption.PureInstances.myOptionTraverse,
      implicitly,
      implicitly,
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

class MyListPureTraverseSpec
    extends TraverseSpec[MyList, Int, String, Long]("MyListPure")(
      MyList.PureInstances.myListTraverse,
      implicitly,
      implicitly,
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

class MyEitherPureTraverseSpec
    extends TraverseSpec[({ type l[X] = MyEither[Int, X] })#l, Int, String, Long]("MyEitherPure")(
      MyEither.PureInstances.myEitherTraverse,
      implicitly,
      implicitly,
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

class MyValidatedPureTraverseSpec
    extends TraverseSpec[({ type l[X] = MyValidated[Int, X] })#l, Int, String, Long]("MyValidatedPure")(
      MyValidated.PureInstances.myValidatedTraverse,
      implicitly,
      implicitly,
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
