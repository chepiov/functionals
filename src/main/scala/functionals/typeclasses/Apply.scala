package functionals.typeclasses

trait Apply[F[_]] extends Any with Functor[F] {
  def ap[A, B](fa: F[A])(ff: F[A => B]): F[B]
}

object Apply {
  def apply[F[_]](implicit F: Apply[F]): Apply[F] = F
}
