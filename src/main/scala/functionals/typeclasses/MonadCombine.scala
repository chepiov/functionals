package functionals.typeclasses

import scala.language.higherKinds

trait MonadCombine[F[_]] extends Any with MonadFilter[F] with MonoidK[F] {
  def unite[G[_]: Foldable, A](fga: F[G[A]]): F[A] =
    flatMap(fga)(ga => Foldable[G].foldMap(ga)(a => pure(a))(toMonoid[A]))
}

object MonadCombine {
  def apply[F[_]](implicit F: MonadCombine[F]): MonadCombine[F] = F
}
