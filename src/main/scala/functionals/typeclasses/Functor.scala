package functionals.typeclasses

import scala.language.higherKinds

trait Functor[F[_]] extends Any { self =>
  import Functor._

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def lift[A, B](f: A => B): F[A] => F[B] =
    fa => map(fa)(f)

  def as[A, B](fa: F[A], b: => B): F[B] =
    map(fa)(_ => b)

  def void[A](fa: F[A]): F[Unit] =
    as(fa, ())

  def compose[G[_]: Functor]: Functor[({ type l[X] = F[G[X]] })#l] =
    new Composite[F, G] {
      def F: Functor[F] = self
      def G: Functor[G] = Functor[G]
    }
}

object Functor {

  def apply[F[_]](implicit F: Functor[F]): Functor[F] = F

  trait Composite[F[_], G[_]] extends Any with Functor[({ type l[X] = F[G[X]] })#l] {

    def F: Functor[F]
    def G: Functor[G]

    def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] =
      F.map(fa)(ga => G.map(ga)(f))
  }

  trait Ops[F[_], A] {
    def typeClassInstance: Functor[F]
    def self: F[A]

    def map[B](f: A => B): F[B] = typeClassInstance.map(self)(f)
    def as[B](b: => B): F[B]    = typeClassInstance.as(self, b)
    def void: F[Unit]           = typeClassInstance.void(self)
  }

  trait ToFunctorOps {
    implicit def toFunctorOps[F[_], A](target: F[A])(implicit tc: Functor[F]): Ops[F, A] = new Ops[F, A] {
      val self: F[A]                    = target
      val typeClassInstance: Functor[F] = tc
    }
  }

  object ops extends ToFunctorOps
}
