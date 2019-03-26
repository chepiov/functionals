package functionals.typeclasses

trait Apply[F[_]] extends Any with Functor[F] {
  def ap[A, B](fa: F[A])(ff: F[A => B]): F[B]
}
