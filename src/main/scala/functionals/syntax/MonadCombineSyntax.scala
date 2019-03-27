package functionals.syntax

import functionals.typeclasses.{Foldable, MonadCombine}

trait MonadCombineOps[F[_], A] {
  def typeClassInstance: MonadCombine[F]
  def self: F[A]
}

trait MonadCombineComposedOps[F[_], G[_], A] {
  def typeClassInstance: MonadCombine[F]
  def composedSelf: F[G[A]]

  def unite(implicit G: Foldable[G]): F[A] = typeClassInstance.unite(composedSelf)
}

trait MonadCombineSyntax {
  implicit def toMonadCombineOps[F[_], A](target: F[A])(implicit tc: MonadCombine[F]): MonadCombineOps[F, A] =
    new MonadCombineOps[F, A] {
      def typeClassInstance: MonadCombine[F] = tc
      def self: F[A]                         = target
    }
  implicit def toComposedMonadCombineOps[F[_], G[_], A](
      target: F[G[A]]
  )(implicit tc: MonadCombine[F]): MonadCombineComposedOps[F, G, A] =
    new MonadCombineComposedOps[F, G, A] {
      def typeClassInstance: MonadCombine[F] = tc
      def composedSelf: F[G[A]]              = target
    }
}

trait MonadCombineAllSyntax extends MonadCombineSyntax with MonadFilterAllSyntax with MonoidKAllSyntax
