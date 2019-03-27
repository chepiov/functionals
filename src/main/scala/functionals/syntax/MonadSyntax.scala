package functionals.syntax

import functionals.typeclasses.Monad

trait MonadOps[F[_], A] {
  def typeClassInstance: Monad[F]
  def self: F[A]

  def flatMap[B](f: A => F[B]): F[B] = typeClassInstance.flatMap(self)(f)
  def >>=[B](f: A => F[B]): F[B]     = flatMap(f)
  def *>[B](fb: F[B]): F[B]          = typeClassInstance.flatMap(self)(_ => typeClassInstance.map(fb)(b => b))
  def <*[B](fb: F[B]): F[A]          = typeClassInstance.flatMap(self)(a => typeClassInstance.map(fb)(_ => a))
}

trait MonadComposedOps[F[_], A] {
  def typeClassInstance: Monad[F]
  def composedSelf: F[F[A]]

  def flatten: F[A] = typeClassInstance.flatten(composedSelf)
}

trait MonadSyntax {
  implicit def toMonadOps[F[_], A](target: F[A])(implicit tc: Monad[F]): MonadOps[F, A] =
    new MonadOps[F, A] {
      def typeClassInstance: Monad[F] = tc
      def self: F[A]                  = target
    }
  implicit def toComposedMonadOps[F[_], A](target: F[F[A]])(implicit tc: Monad[F]): MonadComposedOps[F, A] =
    new MonadComposedOps[F, A] {
      def typeClassInstance: Monad[F] = tc
      def composedSelf: F[F[A]]       = target
    }
}

trait MonadAllSyntax extends MonadSyntax with ApplicativeAllSyntax
