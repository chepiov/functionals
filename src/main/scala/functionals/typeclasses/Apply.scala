package functionals.typeclasses

trait Apply[F[_]] extends Any with Functor[F] {
  def ap[A, B](fa: F[A])(ff: F[A => B]): F[B]
}

object Apply {

  def apply[F[_]](implicit F: Apply[F]): Apply[F] = F

  trait Ops[F[_], A] extends Functor.Ops[F, A] {
    def typeClassInstance: Apply[F]
    def self: F[A]

    def ap[B](ff: F[A => B]): F[B]  = typeClassInstance.ap(self)(ff)
    def <*>[B](ff: F[A => B]): F[B] = ap(ff)
  }

  trait ToApplyOps {
    implicit def toApplyOps[F[_], A](target: F[A])(implicit tc: Apply[F]): Ops[F, A] =
      new Ops[F, A] with Functor.Ops[F, A] {
        def typeClassInstance: Apply[F] = tc
        def self: F[A]                  = target
      }
  }

  object ops extends ToApplyOps
}
