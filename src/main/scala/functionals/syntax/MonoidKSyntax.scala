package functionals.syntax

import functionals.typeclasses.MonoidK

trait MonoidKOps[F[_], A] {
  def typeClassInstance: MonoidK[F]
  def self: F[A]
}

trait MonoidKSyntax {
  implicit def toMonoidKOps[F[_], A](target: F[A])(implicit tc: MonoidK[F]): MonoidKOps[F, A] =
    new MonoidKOps[F, A] {
      def typeClassInstance: MonoidK[F] = tc
      def self: F[A]                    = target
    }
}

trait MonoidKAllSyntax extends MonoidKSyntax with SemigroupKAllSyntax
