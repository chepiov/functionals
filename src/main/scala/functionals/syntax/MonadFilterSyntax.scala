package functionals.syntax

import functionals.typeclasses.MonadFilter

trait MonadFilterOps[F[_], A] {
  def typeClassInstance: MonadFilter[F]
  def self: F[A]

  def filter(f: A => Boolean): F[A]     = typeClassInstance.filter(self)(f)
  def filterM(f: A => F[Boolean]): F[A] = typeClassInstance.filterM(self)(f)
}

trait MonadFilterSyntax {
  implicit def toMonadFilterOps[F[_], A](target: F[A])(implicit tc: MonadFilter[F]): MonadFilterOps[F, A] =
    new MonadFilterOps[F, A] {
      def typeClassInstance: MonadFilter[F] = tc
      def self: F[A]                        = target
    }
}

trait MonadFilterAllSyntax extends MonadFilterSyntax with MonadAllSyntax
