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

  trait Ops[F[_], A] {
    def typeClassInstance: Foldable[F]
    def self: F[A]

    def foldl[B](z: B)(f: (B, A) => B): B = typeClassInstance.foldLeft(self, z)(f)
    def foldr[B](z: B)(f: (A, B) => B): B = typeClassInstance.foldRight(self, z)(f)
    def foldMap[B: Monoid](f: A => B): B  = typeClassInstance.foldMap(self)(f)
    def fold(implicit M: Monoid[A]): A    = typeClassInstance.fold(self)
    def traverse_[G[_]: Applicative, B](f: A => G[B]): G[Unit] =
      typeClassInstance.traverse_(self)(f)

  }

  trait ComposedOps[F[_], G[_], A] {
    def typeClassInstance: Foldable[F]
    def composedSelf: F[G[A]]

    def sequence_[B](implicit A: Applicative[G]): G[Unit] =
      typeClassInstance.sequence_(composedSelf)
  }

  trait ToFoldableOps {
    implicit def toFoldableOps[F[_], A](target: F[A])(implicit tc: Foldable[F]): Ops[F, A] =
      new Ops[F, A] {
        def typeClassInstance: Foldable[F] = tc
        def self: F[A]                     = target
      }
    implicit def toComposedFoldableOps[F[_], G[_], A](target: F[G[A]])(implicit tc: Foldable[F]): ComposedOps[F, G, A] =
      new ComposedOps[F, G, A] {
        def typeClassInstance: Foldable[F] = tc
        def composedSelf: F[G[A]]          = target
      }
  }

  object ops {
    implicit class Syntax[F[_], A](fa: F[A]) {
      def foldl[B](z: B)(f: (B, A) => B)(implicit F: Foldable[F]): B = F.foldLeft(fa, z)(f)
      def foldr[B](z: B)(f: (A, B) => B)(implicit F: Foldable[F]): B = F.foldRight(fa, z)(f)
      def foldMap[B: Monoid](f: A => B)(implicit F: Foldable[F]): B  = F.foldMap(fa)(f)
      def fold(implicit M: Monoid[A], F: Foldable[F]): A             = F.fold(fa)
      def traverse_[G[_]: Applicative, B](f: A => G[B])(implicit F: Foldable[F]): G[Unit] =
        F.traverse_(fa)(f)
    }
    implicit class SeqSyntax[F[_], G[_], A](fga: F[G[A]]) {
      def sequence_[B](implicit A: Applicative[G], F: Foldable[F]): G[Unit] =
        F.sequence_(fga)
    }
  }
}
