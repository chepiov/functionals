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
}
