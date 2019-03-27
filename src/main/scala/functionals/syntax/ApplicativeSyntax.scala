package functionals.syntax

import functionals.typeclasses.Applicative

trait ApplicativeOps[F[_], A] {
  def typeClassInstance: Applicative[F]
  def self: F[A]
}

trait ApplicativeSyntax {
  implicit def toApplicativeOps[F[_], A](target: F[A])(implicit tc: Applicative[F]): ApplicativeOps[F, A] =
    new ApplicativeOps[F, A] {
      def typeClassInstance: Applicative[F] = tc
      def self: F[A]                        = target
    }
}
