package functionals.typeclasses

trait Monoid[A] extends Any with Semigroup[A] {
  def empty: A
}

object Monoid {

  def apply[A](implicit F: Monoid[A]): Monoid[A] = F

  object ops {
    implicit class Syntax[A](x: A) extends Semigroup.ops.Syntax[A](x)
  }

  def instance[A](ea: A)(f: (A, => A) => A): Monoid[A] =
    new Monoid[A] {
      override def empty: A               = ea
      override def combine(x: A, y: A): A = f(x, y)
    }
}
