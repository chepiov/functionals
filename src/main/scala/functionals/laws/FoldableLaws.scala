package functionals.laws

import functionals.typeclasses.{Foldable, Monoid}

import scala.language.higherKinds

trait FoldableLaws[F[_]] {
  import functionals.syntax.all._
  import IsEqual.ops._

  implicit val typeClass: Foldable[F]

  def foldableLeftConsistenceWithFoldMap[A, B: Monoid](fa: F[A], f: A => B): IsEqual[B] =
    fa.foldMap(f) =?= fa.foldl(Monoid[B].empty)((b, a) => Monoid[B].combine(b, f(a)))

  def foldableRightConsistenceWithFoldMap[A, B: Monoid](fa: F[A], f: A => B): IsEqual[B] =
    fa.foldMap(f) =?= fa.foldr(Monoid[B].empty)((a, b) => Monoid[B].combine(f(a), b))

  def foldRef[A: Monoid](fa: F[A]): IsEqual[A] =
    typeClass.fold(fa) =?= typeClass.foldLeft(fa, Monoid[A].empty)((acc, a) => Monoid[A].combine(acc, a))

}

object FoldableLaws {
  def apply[F[_]: Foldable]: FoldableLaws[F] = new FoldableLaws[F] {
    val typeClass: Foldable[F] = Foldable[F]
  }
}
