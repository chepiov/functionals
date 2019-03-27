package functionals.laws

import functionals.typeclasses.Semigroup

trait SemigroupLaws[A] {
  import IsEqual.ops._
  import functionals.syntax.withParent.semigroup._

  implicit val typeClass: Semigroup[A]

  def combineAssociativity(x: A, y: A, z: A): IsEqual[A] =
    ((x |+| y) |+| z) =?= (x |+| (y |+| z))
}

object SemigroupLaws {
  def apply[A: Semigroup]: SemigroupLaws[A] = new SemigroupLaws[A] {
    val typeClass: Semigroup[A] = Semigroup[A]
  }
}
