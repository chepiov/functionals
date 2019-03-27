package functionals.syntax

import functionals.typeclasses.{Applicative, Traverse}

trait TraverseOps[F[_], A] {
  def typeClassInstance: Traverse[F]
  def self: F[A]

  def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] = typeClassInstance.traverse(self)(f)
}

trait TraverseComposedOps[F[_], G[_], A] {
  def typeClassInstance: Traverse[F]
  def composedSelf: F[G[A]]

  def sequence(implicit A: Applicative[G]): G[F[A]] = typeClassInstance.sequence(composedSelf)
}

trait TraverseSyntax {
  implicit def toTraverseOps[F[_], A](target: F[A])(implicit tc: Traverse[F]): TraverseOps[F, A] =
    new TraverseOps[F, A] {
      def typeClassInstance: Traverse[F] = tc
      def self: F[A]                     = target
    }
  implicit def toComposedTraverseOps[F[_], G[_], A](
      target: F[G[A]]
  )(implicit tc: Traverse[F]): TraverseComposedOps[F, G, A] =
    new TraverseComposedOps[F, G, A] {
      def typeClassInstance: Traverse[F] = tc
      def composedSelf: F[G[A]]          = target
    }
}
