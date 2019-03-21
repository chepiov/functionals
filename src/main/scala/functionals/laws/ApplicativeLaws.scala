package functionals.laws

import functionals.typeclasses.Applicative

import scala.language.higherKinds

trait ApplicativeLaws[F[_]] extends FunctorLaws[F] {
  import Applicative.ops._
  import IsEqual.ops._

  implicit val typeClass: Applicative[F]

  def identity[A](fa: F[A]): IsEqual[F[A]] =
    fa.ap(typeClass.pure[A => A](a => a)) =?= fa

  def homomorphism[A, B](a: A, f: A => B): IsEqual[F[B]] =
    typeClass.pure(a).ap(typeClass.pure(f)) =?= typeClass.pure(f(a))

  def interchange[A, B](a: A, ff: F[A => B]): IsEqual[F[B]] =
    typeClass.pure(a).ap(ff) =?= ff.ap(typeClass.pure((f: A => B) => f(a)))

  def mapConsistence[A, B](fa: F[A], f: A => B): IsEqual[F[B]] =
    fa.map(f) =?= fa.ap(typeClass.pure(f))
}

object ApplicativeLaws {
  def apply[F[_]: Applicative]: ApplicativeLaws[F] = new ApplicativeLaws[F] {
    val typeClass: Applicative[F] = Applicative[F]
  }
}
