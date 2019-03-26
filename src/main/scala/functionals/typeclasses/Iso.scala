package functionals.typeclasses

trait Iso[A, B] {
  def wrap(a: A): B
  def unwrap(b: B): A
}

object Iso {
  trait Wrapper[T] extends Any {
    def value: T
  }

  trait WrapperCompanion[F[x] <: Wrapper[x]] {
    def apply[T](x: T): F[T]
    implicit def wrapperIso[T]: Iso[T, F[T]] = new Iso[T, F[T]] {
      def wrap(a: T): F[T]   = apply(a)
      def unwrap(b: F[T]): T = b.value
    }
  }
}
