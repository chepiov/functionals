package functionals.typeclasses

trait Semigroup[A] extends Any {
  def combine(x: A, y: A): A
}

object Semigroup {
  def apply[A](implicit F: Semigroup[A]): Semigroup[A] = F
}
