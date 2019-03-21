package functionals.typeclasses

trait Equal[A] {
  def equal(x: A, y: A): Boolean
  final def notEqual(x: A, y: A): Boolean = !equal(x, y)
}

case object Equal {
  def natural[A]: Equal[A] = new Equal[A] {
    def equal(x: A, y: A): Boolean = x == y
  }

  def instance[A](f: (A, A) => Boolean): Equal[A] = new Equal[A] {
    def equal(x: A, y: A): Boolean = f(x, y)
  }

  object ops {
    implicit class Syntax[A](x: A) {
      def ===(y: A)(implicit E: Equal[A]): Boolean = E.equal(x, y)
      def =!=(y: A)(implicit E: Equal[A]): Boolean = E.notEqual(x, y)
    }
  }

  def apply[A](implicit A: Equal[A]): Equal[A] = A
}
