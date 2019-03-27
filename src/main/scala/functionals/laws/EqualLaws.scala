package functionals.laws

import functionals.typeclasses.Equal

trait EqualLaws[A] {
  import functionals.syntax.withParent.equal._
  import IsEqual.ops._

  implicit val typeClass: Equal[A]

  def symmetric(x: A, y: A): IsEqual[Boolean] =
    (x === y) =?= (y === x)

  def antiSymmetric(x: A, y: A): IsEqual[Boolean] =
    (x =!= y) =?= (y =!= x)

}

object EqualLaws {
  def apply[A: Equal]: EqualLaws[A] = new EqualLaws[A] {
    val typeClass: Equal[A] = Equal[A]
  }
}
