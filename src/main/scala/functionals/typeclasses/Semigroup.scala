package functionals.typeclasses

trait Semigroup[A] extends Any {
  def combine(x: A, y: A): A
}

object Semigroup {

  def apply[A](implicit F: Semigroup[A]): Semigroup[A] = F

  object ops {
    implicit class Syntax[A](x: A) {
      def |+|(y: A)(implicit A: Semigroup[A]): A = A.combine(x, y)
    }
  }
}
