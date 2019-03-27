package functionals.syntax

import functionals.typeclasses.Equal

trait EqualOps[A] {
  def typeClassInstance: Equal[A]
  def self: A

  def ===(y: A): Boolean = typeClassInstance.equal(self, y)
  def =!=(y: A): Boolean = typeClassInstance.notEqual(self, y)
}

trait EqualSyntax {
  implicit def toEqualOps[A](target: A)(implicit tc: Equal[A]): EqualOps[A] =
    new EqualOps[A] {
      def typeClassInstance: Equal[A] = tc
      def self: A                     = target
    }
}
