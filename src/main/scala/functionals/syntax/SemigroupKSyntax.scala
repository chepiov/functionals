package functionals.syntax

import functionals.typeclasses.SemigroupK

trait SemigroupKOps[F[_], A] {
  def typeClassInstance: SemigroupK[F]
  def self: F[A]

  def <+>(y: F[A]): F[A] = typeClassInstance.combine(self, y)
}

trait SemigroupKSyntax {
  implicit def toSemigroupKOps[F[_], A](target: F[A])(implicit tc: SemigroupK[F]): SemigroupKOps[F, A] =
    new SemigroupKOps[F, A] {
      def typeClassInstance: SemigroupK[F] = tc
      def self: F[A]                       = target
    }
}

trait SemigroupKAllSyntax extends SemigroupKSyntax
