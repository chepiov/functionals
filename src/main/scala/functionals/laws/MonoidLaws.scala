package functionals.laws

import functionals.typeclasses.Monoid

trait MonoidLaws[A] extends SemigroupLaws[A] {
  import IsEqual.ops._
  import Monoid.ops._

  implicit val typeClass: Monoid[A]

  def combineRightIdentity(x: A): IsEqual[A] =
    (x |+| typeClass.empty) =?= x

  def combineLeftIdentity(x: A): IsEqual[A] =
    (typeClass.empty |+| x) =?= x
}

object MonoidLaws {
  def apply[A: Monoid]: MonoidLaws[A] = new MonoidLaws[A] {
    val typeClass: Monoid[A] = Monoid[A]
  }
}
