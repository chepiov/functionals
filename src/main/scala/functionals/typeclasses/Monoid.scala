package functionals.typeclasses

trait Monoid[A] extends Any with Semigroup[A] {
  def empty: A
}

object Monoid {

  def apply[A](implicit F: Monoid[A]): Monoid[A] = F

  trait Ops[A] extends Semigroup.Ops[A] {
    def typeClassInstance: Monoid[A]
    def empty: A = typeClassInstance.empty
  }

  trait ToMonoidOps {
    implicit def toMonoidOps[A](target: A)(implicit tc: Monoid[A]): Ops[A] = new Ops[A] {
      val self: A                      = target
      val typeClassInstance: Monoid[A] = tc
    }
  }

  object ops extends ToMonoidOps

  def instance[A](ea: A)(f: (A, => A) => A): Monoid[A] =
    new Monoid[A] {
      override def empty: A               = ea
      override def combine(x: A, y: A): A = f(x, y)
    }
}
