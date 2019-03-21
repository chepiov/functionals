//package functionals.typeclasses
//
//import org.scalacheck.Arbitrary
//
//import scala.language.higherKinds
//
////noinspection ScalaUnnecessaryParentheses
//trait MonadFilterLawsSpec[F[_], A, B, C] extends MonadLawsSpec[F, A, B, C] { _: LawsSpec =>
//
//  implicit def M: MonadFilter[F]
//
//  override lazy val laws: MonadFilterLaws[F] = MonadFilterLaws[F]
//
//  property(s"A MonadFilter[$name] should satisfy filterLeftDistributivity law") {
//    forAll { (f: A => F[B]) =>
//      laws.filterLeftDistributivity(f) shouldBe true
//    }
//  }
//
//  property(s"A MonadFilter[$name] should satisfy filterRightDistributivity law") {
//    forAll { fa: F[A] =>
//      laws.filterRightDistributivity(fa) shouldBe true
//    }
//  }
//}
//
//abstract class MonadFilterSpec[F[_], A, B, C](val name: String)(implicit val M: MonadFilter[F],
//                                                                val arbFa: Arbitrary[F[A]],
//                                                                val arbAB: Arbitrary[A => B],
//                                                                val arbBC: Arbitrary[B => C],
//                                                                val arbA: Arbitrary[A],
//                                                                val arbFAB: Arbitrary[F[A => B]],
//                                                                val arbAFB: Arbitrary[A => F[B]],
//                                                                val arbBFC: Arbitrary[B => F[C]])
//    extends LawsSpec with MonadFilterLawsSpec[F, A, B, C] {
//  val F: Functor[F]     = M
//  val A: Applicative[F] = M
//}
