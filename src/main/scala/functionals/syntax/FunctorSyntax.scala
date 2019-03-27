package functionals.syntax

import functionals.typeclasses.Functor

trait FunctorOps[F[_], A] {
  def typeClassInstance: Functor[F]
  def self: F[A]

  def map[B](f: A => B): F[B] = typeClassInstance.map(self)(f)
  def as[B](b: => B): F[B]    = typeClassInstance.as(self, b)
  def void: F[Unit]           = typeClassInstance.void(self)
}

trait FunctorSyntax {
  implicit def toFunctorOps[F[_], A](target: F[A])(implicit tc: Functor[F]): FunctorOps[F, A] = new FunctorOps[F, A] {
    val self: F[A]                    = target
    val typeClassInstance: Functor[F] = tc
  }
}

trait FunctorAllSyntax extends FunctorSyntax
