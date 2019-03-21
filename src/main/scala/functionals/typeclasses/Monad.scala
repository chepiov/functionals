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

  object ops {
    implicit class Syntax[F[_], A](fa: F[A]) extends Applicative.ops.Syntax(fa) {
      def flatMap[B](f: A => F[B])(implicit M: Monad[F]): F[B] = M.flatMap(fa)(f)
    }
    implicit class FlattenSyntax[F[_], A](ffa: F[F[A]]) {
      def flatten(implicit M: Monad[F]): F[A] = M.flatten(ffa)
    }
  }
}
