package functionals.typeclasses

import scala.language.higherKinds

trait Monad[F[_]] extends Any with Applicative[F] { self =>

  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] =
    flatMap(ff)(f => map(fa)(f))

  def flatten[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(identity)

  // Not-mandatory, already implemented in terms of pure/ap in Applicative, just for performance
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))
}

object Monad {

  def apply[F[_]](implicit F: Monad[F]): Monad[F] = F

  trait Ops[F[_], A] extends Applicative.Ops[F, A] {
    def typeClassInstance: Monad[F]
    def self: F[A]

    def flatMap[B](f: A => F[B]): F[B] = typeClassInstance.flatMap(self)(f)
  }

  trait ComposedOps[F[_], A] {
    def typeClassInstance: Monad[F]
    def composedSelf: F[F[A]]

    def flatten: F[A] = typeClassInstance.flatten(composedSelf)
  }

  trait ToMonadOps {
    implicit def toMonadOps[F[_], A](target: F[A])(implicit tc: Monad[F]): Ops[F, A] =
      new Ops[F, A] {
        def typeClassInstance: Monad[F] = tc
        def self: F[A]                  = target
      }
    implicit def toComposedMonadOps[F[_], A](target: F[F[A]])(implicit tc: Monad[F]): ComposedOps[F, A] =
      new ComposedOps[F, A] {
        def typeClassInstance: Monad[F] = tc
        def composedSelf: F[F[A]]       = target
      }
  }

  object ops extends ToMonadOps
}
