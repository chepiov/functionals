package functionals.laws

import functionals.typeclasses.Monad

import scala.language.higherKinds

trait MonadLaws[F[_]] extends ApplicativeLaws[F] {
  import IsEqual.ops._
  import functionals.syntax.all._

  implicit val typeClass: Monad[F]

  def flatMapAssociativity[A, B, C](fa: F[A], f: A => F[B], g: B => F[C]): IsEqual[F[C]] =
    fa.flatMap(f).flatMap(g) =?= fa.flatMap(a => f(a).flatMap(b => g(b)))

  def leftIdentity[A, B](a: A, f: A => F[B]): IsEqual[F[B]] =
    typeClass.pure(a).flatMap(f) =?= f(a)

  def rightIdentity[A](fa: F[A]): IsEqual[F[A]] =
    fa.flatMap(a => typeClass.pure(a)) =?= fa
}

object MonadLaws {
  def apply[F[_]: Monad]: MonadLaws[F] = new MonadLaws[F] {
    val typeClass: Monad[F] = Monad[F]
  }
}
