package functionals.laws

import functionals.typeclasses.MonadFilter

import scala.language.higherKinds

trait MonadFilterLaws[F[_]] extends MonadLaws[F] {

  implicit val typeClass: MonadFilter[F]

  import MonadFilter.ops._
  import typeClass.empty

  def filterLeftDistributivity[A, B](f: A => F[B]): Boolean =
    empty[A].flatMap(f) == empty[B]

  def filterRightDistributivity[A](fa: F[A]): Boolean =
    fa.flatMap(_ => empty[A]) == empty[A]
}

object MonadFilterLaws {
  def apply[F[_]: MonadFilter]: MonadFilterLaws[F] = new MonadFilterLaws[F] {
    val typeClass: MonadFilter[F] = MonadFilter[F]
  }
}
