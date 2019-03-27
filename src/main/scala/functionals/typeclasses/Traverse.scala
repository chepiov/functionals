package functionals.typeclasses

import scala.language.higherKinds

trait Traverse[F[_]] extends Any with Foldable[F] with Functor[F] { self =>
  import Traverse._

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_]: Applicative, A](fa: F[G[A]]): G[F[A]] =
    traverse(fa)(identity)

  def compose[G[_]: Traverse]: Traverse[({ type l[X] = F[G[X]] })#l] =
    new Composite[F, G] {
      def F: Traverse[F] = self
      def G: Traverse[G] = Traverse[G]
    }
}

object Traverse {

  def apply[F[_]](implicit F: Traverse[F]): Traverse[F] = F

  trait Composite[F[_], G[_]]
      extends Any with Traverse[({ type l[X] = F[G[X]] })#l] with Foldable.Composite[F, G]
      with Functor.Composite[F, G] {

    def F: Traverse[F]
    def G: Traverse[G]

    def traverse[H[_]: Applicative, A, B](fa: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
      F.traverse(fa)(ga => G.traverse(ga)(f))
  }
}
