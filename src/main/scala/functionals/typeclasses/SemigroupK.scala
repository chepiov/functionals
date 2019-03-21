package functionals.typeclasses

import scala.language.higherKinds

trait SemigroupK[F[_]] extends Any { self =>

  def combine[A](x: F[A], y: F[A]): F[A]

  def toSemigroup[A]: Semigroup[F[A]] =
    new Semigroup[F[A]] {
      def combine(x: F[A], y: F[A]): F[A] = self.combine(x, y)
    }
}

object SemigroupK {

  def apply[F[_]](implicit F: SemigroupK[F]): SemigroupK[F] = F

  object ops {
    implicit class Syntax[F[_], A](x: F[A]) {
      def <+>(y: F[A])(implicit S: SemigroupK[F]): F[A] = S.combine(x, y)
    }
  }
}
