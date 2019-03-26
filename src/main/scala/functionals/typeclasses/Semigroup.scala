package functionals.typeclasses

trait Semigroup[A] extends Any {
  def combine(x: A, y: A): A
}

object Semigroup {

  def apply[A](implicit F: Semigroup[A]): Semigroup[A] = F

  trait Ops[A] {
    def typeClassInstance: Semigroup[A]
    def self: A
    def |+|(y: A): A = typeClassInstance.combine(self, y)
  }

  trait ToSemigroupOps {
    implicit def toSemigroupOps[A](target: A)(implicit tc: Semigroup[A]): Ops[A] = new Ops[A] {
      val self: A                         = target
      val typeClassInstance: Semigroup[A] = tc
    }
  }

  object ops extends ToSemigroupOps
}
