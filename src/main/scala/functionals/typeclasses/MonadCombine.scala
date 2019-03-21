package functionals.typeclasses

import scala.language.higherKinds

trait MonadCombine[F[_]] extends Any with MonadFilter[F] with MonoidK[F] {
  def unite[G[_]: Foldable, A](fga: F[G[A]]): F[A] =
    flatMap(fga)(ga => Foldable[G].foldMap(ga)(a => pure(a))(toMonoid[A]))
}

object MonadCombine {

  def apply[F[_]](implicit F: MonadCombine[F]): MonadCombine[F] = F

  object ops {
    implicit class MonadFilterSyntax[F[_], A](fa: F[A]) extends MonadFilter.ops.Syntax(fa)
    implicit class MonoidKSyntax[F[_], A](fa: F[A])     extends MonoidK.ops.Syntax(fa)
    implicit class Syntax[F[_], G[_], A](fga: F[G[A]]) {
      def unite(implicit F: MonadCombine[F], G: Foldable[G]): F[A] = F.unite(fga)
    }
  }
}

