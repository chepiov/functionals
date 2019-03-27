package functionals.syntax

import functionals.typeclasses.Apply

trait ApplyOps[F[_], A] {
  def typeClassInstance: Apply[F]
  def self: F[A]

  def ap[B](ff: F[A => B]): F[B]  = typeClassInstance.ap(self)(ff)
  def <*>[B](ff: F[A => B]): F[B] = ap(ff)
}

trait ApplySyntax {
  implicit def toApplyOps[F[_], A](target: F[A])(implicit tc: Apply[F]): ApplyOps[F, A] =
    new ApplyOps[F, A] {
      def typeClassInstance: Apply[F] = tc
      def self: F[A]                  = target
    }
}
