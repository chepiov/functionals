//package functionals.typeclasses
//
//import functionals.structures.{MyEither, MyList, MyOption, MyValidated}
//import org.scalacheck.Arbitrary
//
//import scala.language.{higherKinds, reflectiveCalls}
//
//trait SemigroupKLawsSpec[F[_], A] { _: LawsSpec =>
//
//  implicit def S: SemigroupK[F]
//  implicit def arbFa: Arbitrary[F[A]]
//
//  def name: String
//
//  lazy val laws: SemigroupKLaws[F] = SemigroupKLaws[F]
//
//  property(s"A SemigroupK[$name] should satisfy combine associativity law") {
//    forAll { (x: F[A], y: F[A], z: F[A]) =>
//      laws.combineAssociativity(x, y, z) shouldBe true
//    }
//  }
//}
//
//abstract class SemigroupKSpec[F[_], A](val name: String)(implicit val S: SemigroupK[F], val arbFa: Arbitrary[F[A]])
//    extends LawsSpec with SemigroupKLawsSpec[F, A]
//
//class MyListSemigroupKSpec      extends SemigroupKSpec[MyList, Int]("MyList")
//class MyOptionSemigroupKSpec    extends SemigroupKSpec[MyOption, Int]("MyOption")
//class MyEitherSemigroupKSpec    extends SemigroupKSpec[({ type l[X] = MyEither[Int, X] })#l, Int]("MyEither")
//class MyValidatedSemigroupKSpec extends SemigroupKSpec[({ type l[X] = MyValidated[Int, X] })#l, Int]("MyValidated")
//
//class MyListPureSemigroupKSpec
//    extends SemigroupKSpec[MyList, Int]("MyListPure")(MyList.PureInstances.myListSemigroupK, implicitly)
//class MyOptionPureSemigroupKSpec
//    extends SemigroupKSpec[MyOption, Int]("MyOptionPure")(MyOption.PureInstances.myOptionSemigroupK, implicitly)
//class MyEitherPureSemigroupKSpec
//    extends SemigroupKSpec[({ type l[X] = MyEither[Int, X] })#l, Int]("MyEitherPure")(MyEither.PureInstances.semigroupK,
//                                                                                      implicitly)
//class MyValidatedPureSemigroupKSpec
//    extends SemigroupKSpec[({ type l[X] = MyValidated[Int, X] })#l, Int]("MyValidatedPure")(
//      MyValidated.PureInstances.myValidatedSemigroupK,
//      implicitly)
