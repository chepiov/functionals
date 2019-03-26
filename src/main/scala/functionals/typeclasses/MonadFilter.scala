package functionals.typeclasses

import scala.language.higherKinds

trait MonadFilter[F[_]] extends Any with Monad[F] {

  def empty[A]: F[A]

  def filter[A](fa: F[A])(f: A => Boolean): F[A] =
    flatMap(fa)(a => if (f(a)) pure(a) else empty[A])

  def filterM[A](fa: F[A])(f: A => F[Boolean]): F[A] =
    flatMap(fa)(a => flatMap(f(a))(b => if (b) pure(a) else empty[A]))
}

object MonadFilter {

  def apply[F[_]](implicit F: MonadFilter[F]): MonadFilter[F] = F

  trait Ops[F[_], A] extends Monad.Ops[F, A] {
    def typeClassInstance: MonadFilter[F]
    def self: F[A]

    def filter(f: A => Boolean): F[A]     = typeClassInstance.filter(self)(f)
    def filterM(f: A => F[Boolean]): F[A] = typeClassInstance.filterM(self)(f)
  }

  trait ToMonadFilterOps {
    implicit def toMonadFilterOps[F[_], A](target: F[A])(implicit tc: MonadFilter[F]): Ops[F, A] =
      new Ops[F, A] {
        def typeClassInstance: MonadFilter[F] = tc
        def self: F[A]                        = target
      }
  }

  object ops extends ToMonadFilterOps
}
