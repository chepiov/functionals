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
}
