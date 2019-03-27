package functionals.syntax

import functionals.typeclasses.{Applicative, Foldable, Monoid}

trait FoldableOps[F[_], A] {
  def typeClassInstance: Foldable[F]
  def self: F[A]

  def foldl[B](z: B)(f: (B, A) => B): B = typeClassInstance.foldLeft(self, z)(f)
  def foldr[B](z: B)(f: (A, B) => B): B = typeClassInstance.foldRight(self, z)(f)
  def foldMap[B: Monoid](f: A => B): B  = typeClassInstance.foldMap(self)(f)
  def fold(implicit M: Monoid[A]): A    = typeClassInstance.fold(self)
  def traverse_[G[_]: Applicative, B](f: A => G[B]): G[Unit] =
    typeClassInstance.traverse_(self)(f)

}

trait FoldableComposedOps[F[_], G[_], A] {
  def typeClassInstance: Foldable[F]
  def composedSelf: F[G[A]]

  def sequence_[B](implicit A: Applicative[G]): G[Unit] =
    typeClassInstance.sequence_(composedSelf)
}

trait FoldableSyntax {
  implicit def toFoldableOps[F[_], A](target: F[A])(implicit tc: Foldable[F]): FoldableOps[F, A] =
    new FoldableOps[F, A] {
      def typeClassInstance: Foldable[F] = tc
      def self: F[A]                     = target
    }
  implicit def toComposedFoldableOps[F[_], G[_], A](
      target: F[G[A]]
  )(implicit tc: Foldable[F]): FoldableComposedOps[F, G, A] =
    new FoldableComposedOps[F, G, A] {
      def typeClassInstance: Foldable[F] = tc
      def composedSelf: F[G[A]]          = target
    }
}

trait FoldableAllSyntax extends FoldableSyntax
