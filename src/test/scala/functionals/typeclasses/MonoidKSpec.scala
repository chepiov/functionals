//package functionals.typeclasses
//
//import functionals.structures.{MyList, MyOption}
//import org.scalacheck.Arbitrary
//
//import scala.language.higherKinds
//
//trait MonoidKLawSpec[F[_], A] extends SemigroupKLawsSpec[F, A] { _: LawsSpec =>
//
//  implicit val M: MonoidK[F]
//
//  def name: String
//
//  override lazy val laws: MonoidKLaws[F] = MonoidKLaws[F]
//
//  property(s"A MonoidK[$name] should satisfy combine right identity law") {
//    forAll { fa: F[A] =>
//      laws.combineRightIdentity(fa) shouldBe true
//    }
//  }
//
//  property(s"A MonoidK[$name] should satisfy combine left identity law") {
//    forAll { fa: F[A] =>
//      laws.combineLeftIdentity(fa) shouldBe true
//    }
//  }
//}
//
//abstract class MonoidKSpec[F[_], A](val name: String)(implicit val M: MonoidK[F], val arbFa: Arbitrary[F[A]])
//    extends LawsSpec with MonoidKLawSpec[F, A] {
//  val S: SemigroupK[F] = M
//}
//
//class MyListMonoidKSpec   extends MonoidKSpec[MyList, Int]("MyList")
//class MyOptionMonoidKSpec extends MonoidKSpec[MyOption, Int]("MyOption")
//
//class MyListPureMonoidKSpec extends MonoidKSpec[MyList, Int]("MyList")(MyList.PureInstances.myListMonoidK, implicitly)
//class MyOptionPureMonoidKSpec
//    extends MonoidKSpec[MyOption, Int]("MyOption")(MyOption.PureInstances.myOptionMonoidK, implicitly)
