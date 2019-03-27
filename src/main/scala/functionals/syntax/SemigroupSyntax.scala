package functionals.syntax

import functionals.typeclasses.Semigroup

trait SemigroupOps[A] {
  def typeClassInstance: Semigroup[A]
  def self: A

  def |+|(y: A): A = typeClassInstance.combine(self, y)
}

trait SemigroupSyntax {
  implicit def toSemigroupOps[A](target: A)(implicit tc: Semigroup[A]): SemigroupOps[A] = new SemigroupOps[A] {
    val self: A                         = target
    val typeClassInstance: Semigroup[A] = tc
  }
}
