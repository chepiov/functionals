package functionals.laws

import functionals.structures.{MyEither, MyList, MyOption, MyValidated}
import functionals.transformers.{MyEitherT, MyOptionT}
import functionals.typeclasses.Equal
import org.scalacheck.Arbitrary

trait EqualLawsSpec[F] { _: LawsSpec =>

  def name: String

  implicit def F: Equal[F]

  implicit def arbF: Arbitrary[F]

  lazy val laws: EqualLaws[F] = EqualLaws[F]

  property(s"An Equal[$name] should satisfy symmetric law") {
    forAll { (x: F, y: F) =>
      laws.symmetric(x, y).isEqual shouldBe true
    }
  }

  property(s"An Equal[$name] should satisfy anti-symmetric law") {
    forAll { (x: F, y: F) =>
      laws.antiSymmetric(x, y).isEqual shouldBe true
    }
  }
}

abstract class EqualSpec[F](val name: String)(implicit val F: Equal[F], val arbF: Arbitrary[F])
    extends LawsSpec with EqualLawsSpec[F]

class MyByteEqualSpec    extends EqualSpec[Byte]("Byte")
class MyShortEqualSpec   extends EqualSpec[Short]("Short")
class MyIntEqualSpec     extends EqualSpec[Int]("Int")
class MyLongEqualSpec    extends EqualSpec[Long]("Long")
class MyStringEqualSpec  extends EqualSpec[String]("String")
class MyBooleanEqualSpec extends EqualSpec[Boolean]("Boolean")

class MyListEqualSpec      extends EqualSpec[MyList[Int]]("MyList")
class MyOptionEqualSpec    extends EqualSpec[MyOption[Int]]("MyOption")
class MyEitherEqualSpec    extends EqualSpec[MyEither[Int, String]]("MyEither")
class MyValidatedEqualSpec extends EqualSpec[MyValidated[Int, String]]("MyValidated")
class MyOptionTEqualSpec   extends EqualSpec[MyOptionT[MyList, Int]]("MyOptionT")
class MyEitherTEqualSpec   extends EqualSpec[MyEitherT[MyList, String, Int]]("MyEitherT")
