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

  trait Ops[F[_], A] {
    def typeClassInstance: SemigroupK[F]
    def self: F[A]

    def <+>(y: F[A]): F[A] = typeClassInstance.combine(self, y)
  }

  trait ToSemigroupKOps {
    implicit def toSemigroupKOps[F[_], A](target: F[A])(implicit tc: SemigroupK[F]): Ops[F, A] =
      new Ops[F, A] {
        def typeClassInstance: SemigroupK[F] = tc
        def self: F[A]                       = target
      }
  }

  object ops extends ToSemigroupKOps
}
