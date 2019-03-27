package functionals.laws

import functionals.typeclasses.Functor

import scala.language.higherKinds

trait FunctorLaws[F[_]] {
  import functionals.syntax.withParent.functor._
  import IsEqual.ops._

  implicit val typeClass: Functor[F]

  def functorIdentity[A](fa: F[A]): IsEqual[F[A]] =
    fa.map(x => x) =?= fa

  def functorComposition[A, B, C](fa: F[A], f: A => B, g: B => C): IsEqual[F[C]] =
    fa.map(f).map(g) =?= fa.map(f andThen g)
}

object FunctorLaws {
  def apply[F[_]: Functor]: FunctorLaws[F] = new FunctorLaws[F] {
    val typeClass: Functor[F] = Functor[F]
  }
}
