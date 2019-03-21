package functionals.typeclasses

import scala.language.higherKinds

trait MonoidK[F[_]] extends Any with SemigroupK[F] { self =>

  def empty[A]: F[A]

  def toMonoid[A]: Monoid[F[A]] = new Monoid[F[A]] {
    def empty: F[A]                     = self.empty
    def combine(x: F[A], y: F[A]): F[A] = self.combine(x, y)
  }
}

object MonoidK {

  def apply[F[_]](implicit F: MonoidK[F]): MonoidK[F] = F

  object ops {
    implicit class Syntax[F[_], A](fa: F[A]) extends SemigroupK.ops.Syntax[F, A](fa)
  }
}
