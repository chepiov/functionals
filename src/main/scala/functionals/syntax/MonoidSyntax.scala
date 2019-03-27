package functionals.syntax

import functionals.typeclasses.Monoid

trait MonoidOps[A] {
  def typeClassInstance: Monoid[A]
  def self: A
}

trait MonoidSyntax {
  implicit def toMonoidOps[A](target: A)(implicit tc: Monoid[A]): MonoidOps[A] = new MonoidOps[A] {
    val self: A                      = target
    val typeClassInstance: Monoid[A] = tc
  }
}

trait MonoidAllSyntax extends MonoidSyntax with SemigroupAllSyntax
