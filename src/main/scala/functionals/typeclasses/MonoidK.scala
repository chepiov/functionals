package functionals.typeclasses

import scala.language.higherKinds

trait MonoidK[F[_]] extends Any with SemigroupK[F] { self =>

  def empty[A]: F[A]

  def toMonoid[A]: Monoid[F[A]] = new Monoid[F[A]] {
    def empty: F[A]                     = self.empty
    def combine(x: F[A], y: F[A]): F[A] = self.combine(x, y)
  }
}

object MonoidK {

  def apply[F[_]](implicit F: MonoidK[F]): MonoidK[F] = F

  trait Ops[F[_], A] extends SemigroupK.Ops[F, A] {
    def typeClassInstance: MonoidK[F]
    def self: F[A]
  }

  trait ToMonoidKOps {
    implicit def toMonoidKOps[F[_], A](target: F[A])(implicit tc: MonoidK[F]): Ops[F, A] =
      new Ops[F, A] {
        def typeClassInstance: MonoidK[F] = tc
        def self: F[A]                    = target
      }
  }

  object ops extends ToMonoidKOps
}
