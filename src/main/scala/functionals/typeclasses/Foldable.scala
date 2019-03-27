package functionals.typeclasses

import scala.language.higherKinds

trait Foldable[F[_]] extends Any { self =>
  import Foldable._

  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B

  // TODO to Eval
  def foldRight[A, B](fa: F[A], z: B)(f: (A, B) => B): B =
    foldLeft[A, B => B](fa, z => z)((g, a) => b => g(f(a, b)))(z)

  def foldMap[A, B](fa: F[A])(f: A => B)(implicit M: Monoid[B]): B =
    foldLeft(fa, M.empty)((b, a) => M.combine(b, f(a)))

  def fold[A: Monoid](fa: F[A]): A =
    foldMap(fa)(identity)

  def traverse_[G[_], A, B](fa: F[A])(f: A => G[B])(implicit GA: Applicative[G]): G[Unit] =
    foldLeft(fa, GA.pure(()))((acc, a) => GA.map2(acc, f(a))((_, _) => ()))

  def sequence_[G[_]: Applicative, A, B](fga: F[G[A]]): G[Unit] =
    traverse_(fga)(identity)

  def compose[G[_]: Foldable]: Foldable[({ type l[X] = F[G[X]] })#l] =
    new Composite[F, G] {
      def F: Foldable[F] = self
      def G: Foldable[G] = Foldable[G]
    }
}

object Foldable {

  def apply[F[_]](implicit F: Foldable[F]): Foldable[F] = F

  trait Composite[F[_], G[_]] extends Any with Foldable[({ type l[X] = F[G[X]] })#l] {

    def F: Foldable[F]
    def G: Foldable[G]

    def foldLeft[A, B](fa: F[G[A]], z: B)(f: (B, A) => B): B =
      F.foldLeft(fa, z)((b, ga) => G.foldLeft(ga, b)(f))
  }
}
