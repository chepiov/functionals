package functionals.laws

import functionals.typeclasses.MonadCombine

import scala.language.higherKinds

trait MonadCombineLaws[F[_]] extends MonadFilterLaws[F] with MonoidKLaws[F] {

  implicit val typeClass: MonadCombine[F]

  import MonadCombine.ops._

  def monadCombineLeftDistributivity[A, B](fa: F[A], fa2: F[A], f: A => F[B]): Boolean =
    ((fa <+> fa2) flatMap f) == ((fa flatMap f) <+> (fa2 flatMap f))
}

object MonadCombineLaws {
  def apply[F[_]: MonadCombine]: MonadCombineLaws[F] = new MonadCombineLaws[F] {
    val typeClass: MonadCombine[F] = MonadCombine[F]
  }
}
