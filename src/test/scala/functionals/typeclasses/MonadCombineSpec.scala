//package functionals.typeclasses
//
//import org.scalacheck.Arbitrary
//
//import scala.language.higherKinds
//
//trait MonadCombineLawsSpec[F[_], A, B, C] extends MonadFilterLawsSpec[F, A, B, C] with MonoidKLawSpec[F, A] {
//  _: LawsSpec =>
//
//  implicit val M: MonadCombine[F]
//
//  override lazy val laws: MonadCombineLaws[F] = MonadCombineLaws[F]
//
//  property(s"A MonadCombine[$name] should satisfy monadCombineLeftDistributivity law") {
//    forAll { (fa: F[A], fa2: F[A], f: A => F[B]) =>
//      laws.monadCombineLeftDistributivity(fa, fa2, f) shouldBe true
//    }
//  }
//}
//
//abstract class MonadCombineSpec[F[_], A, B, C](val name: String)(implicit val M: MonadCombine[F],
//                                                                 val arbFa: Arbitrary[F[A]],
//                                                                 val arbAB: Arbitrary[A => B],
//                                                                 val arbBC: Arbitrary[B => C],
//                                                                 val arbA: Arbitrary[A],
//                                                                 val arbFAB: Arbitrary[F[A => B]],
//                                                                 val arbAFB: Arbitrary[A => F[B]],
//                                                                 val arbBFC: Arbitrary[B => F[C]])
//    extends LawsSpec with MonadCombineLawsSpec[F, A, B, C] {
//  val F: Functor[F]     = M
//  val A: Applicative[F] = M
//}
