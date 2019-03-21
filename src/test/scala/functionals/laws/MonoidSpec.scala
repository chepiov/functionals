package functionals.laws

import functionals.structures._
import functionals.typeclasses.{Equal, Monoid}
import org.scalacheck.Arbitrary

trait MonoidLawsSpec[A] extends SemigroupLawsSpec[A] { _: LawsSpec =>

  override implicit def F: Monoid[A]

  override lazy val laws: MonoidLaws[A] = MonoidLaws[A]

  property(s"A Monoid[$name] should satisfy combine right identity law") {
    forAll { x: A =>
      laws.combineRightIdentity(x).isEqual shouldBe true
    }
  }

  property(s"A Monoid[$name] should satisfy combine left identity law") {
    forAll { x: A =>
      laws.combineLeftIdentity(x).isEqual shouldBe true
    }
  }
}

abstract class MonoidSpec[A](override val name: String)(implicit val F: Monoid[A],
                                                        val arbA: Arbitrary[A],
                                                        val eqA: Equal[A])
    extends LawsSpec with MonoidLawsSpec[A]

class IntMonoidSpec    extends MonoidSpec[Int]("Int")
class StringMonoidSpec extends MonoidSpec[String]("String")
class ShortMonoidSpec  extends MonoidSpec[Short]("Short")
class ByteMonoidSpec   extends MonoidSpec[Byte]("Byte")
class LongMonoidSpec   extends MonoidSpec[Long]("Long")

class MyOptionMonoidSpec    extends MonoidSpec[MyOption[Int]]("MyOption")
class MyListMonoidSpec      extends MonoidSpec[MyList[Int]]("MyList")
class MyEitherMonoidSpec    extends MonoidSpec[MyEither[String, Int]]("MyEither")
class MyValidatedMonoidSpec extends MonoidSpec[MyValidated[MyList[String], Int]]("MyValidated")

class MyOptionPureMonoidSpec
    extends MonoidSpec[MyOption[Int]]("MyOptionPure")(
      MyOption.PureInstances.myOptionMonoid,
      implicitly,
      implicitly
    )

class MyListPureMonoidSpec
    extends MonoidSpec[MyList[Int]]("MyListPure")(
      MyList.PureInstances.myListMonoid,
      implicitly,
      implicitly
    )

class MyEitherPureMonoidSpec
    extends MonoidSpec[MyEither[String, Int]]("MyEitherPure")(
      MyEither.PureInstances.myEitherMonoid,
      implicitly,
      implicitly
    )

class MyValidatedPureMonoidSpec
    extends MonoidSpec[MyValidated[MyList[String], Int]]("MyValidatedPure")(
      MyValidated.PureInstances.myValidatedMonoid,
      implicitly,
      implicitly
    )
