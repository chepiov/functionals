package functionals.laws

import functionals.typeclasses.MonadFilter

import scala.language.higherKinds

trait MonadFilterLaws[F[_]] extends MonadLaws[F] {
  import IsEqual.ops._
  import functionals.syntax.all._
  import typeClass.empty

  implicit val typeClass: MonadFilter[F]

  def filterLeftDistributivity[A, B](f: A => F[B]): IsEqual[F[B]] = {
    (empty[A] >>= f) =?= empty[B]
  }

  def filterRightDistributivity[A](fa: F[A]): IsEqual[F[A]] =
    (fa >>= (_ => empty[A])) =?= empty[A]
}

object MonadFilterLaws {
  def apply[F[_]: MonadFilter]: MonadFilterLaws[F] = new MonadFilterLaws[F] {
    val typeClass: MonadFilter[F] = MonadFilter[F]
  }
}
