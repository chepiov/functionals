package functionals.typeclasses

import scala.language.higherKinds

trait MonadCombine[F[_]] extends Any with MonadFilter[F] with MonoidK[F] {
  def unite[G[_]: Foldable, A](fga: F[G[A]]): F[A] =
    flatMap(fga)(ga => Foldable[G].foldMap(ga)(a => pure(a))(toMonoid[A]))
}

object MonadCombine {

  def apply[F[_]](implicit F: MonadCombine[F]): MonadCombine[F] = F

  trait Ops[F[_], A] extends MonadFilter.Ops[F, A] with MonoidK.Ops[F, A] {
    def typeClassInstance: MonadCombine[F]
    def self: F[A]
  }

  trait ComposedOps[F[_], G[_], A] {
    def typeClassInstance: MonadCombine[F]
    def composedSelf: F[G[A]]

    def unite(implicit G: Foldable[G]): F[A] = typeClassInstance.unite(composedSelf)
  }

  trait ToMonadCombineOps {
    implicit def toMonadCombineOps[F[_], A](target: F[A])(implicit tc: MonadCombine[F]): Ops[F, A] =
      new Ops[F, A] {
        def typeClassInstance: MonadCombine[F] = tc
        def self: F[A]                         = target
      }
    implicit def toComposedMonadCombineOps[F[_], G[_], A](target: F[G[A]])(
        implicit tc: MonadCombine[F]): ComposedOps[F, G, A] =
      new ComposedOps[F, G, A] {
        def typeClassInstance: MonadCombine[F] = tc
        def composedSelf: F[G[A]]              = target
      }
  }

  object ops extends ToMonadCombineOps
}
