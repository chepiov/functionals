package functionals.laws

import functionals.structures.{MyEither, MyList, MyOption, MyValidated}
import functionals.typeclasses.{Equal, Semigroup}
import org.scalacheck.Arbitrary

trait SemigroupLawsSpec[A] { _: LawsSpec =>

  implicit def F: Semigroup[A]
  implicit def arbA: Arbitrary[A]
  implicit def eqA: Equal[A]

  def name: String

  lazy val laws: SemigroupLaws[A] = SemigroupLaws[A]

  property(s"A Semigroup[$name] should satisfy combine associativity law") {
    forAll { (x: A, y: A, z: A) =>
      laws.combineAssociativity(x, y, z).isEqual should be(true)
    }
  }
}

abstract class SemigroupSpec[A](val name: String)(implicit val F: Semigroup[A],
                                                  val arbA: Arbitrary[A],
                                                  val eqA: Equal[A])
    extends LawsSpec with SemigroupLawsSpec[A]

class IntSemigroupSpec    extends SemigroupSpec[Int]("Int")
class StringSemigroupSpec extends SemigroupSpec[String]("String")
class ShortSemigroupSpec  extends SemigroupSpec[Short]("Short")
class ByteSemigroupSpec   extends SemigroupSpec[Byte]("Byte")
class LongSemigroupSpec   extends SemigroupSpec[Long]("Long")

class MyOptionSemigroupSpec    extends SemigroupSpec[MyOption[Int]]("MyOption")
class MyListSemigroupSpec      extends SemigroupSpec[MyList[Int]]("MyList")
class MyEitherSemigroupSpec    extends SemigroupSpec[MyEither[Int, Int]]("MyEither")
class MyValidatedSemigroupSpec extends SemigroupSpec[MyValidated[Int, Int]]("MyValidated")

class MyOptionPureSemigroupSpec
    extends SemigroupSpec[MyOption[Int]]("MyOptionPure")(
      MyOption.PureInstances.myOptionSemigroup,
      implicitly,
      implicitly
    )

class MyListPureSemigroupSpec
    extends SemigroupSpec[MyList[Int]]("MyListPure")(
      MyList.PureInstances.myListSemigroup,
      implicitly,
      implicitly
    )

class MyEitherPureSemigroupSpec
    extends SemigroupSpec[MyEither[Int, Int]]("MyEitherPure")(
      MyEither.PureInstances.myEitherSemigroup,
      implicitly,
      implicitly
    )

class MyValidatedPureSemigroupSpec
    extends SemigroupSpec[MyValidated[Int, Int]]("MyValidatedPure")(
      MyValidated.PureInstances.myValidatedSemigroup,
      implicitly,
      implicitly
    )
