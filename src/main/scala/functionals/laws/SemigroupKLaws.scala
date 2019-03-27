package functionals.laws

import functionals.typeclasses.SemigroupK

import scala.language.higherKinds

trait SemigroupKLaws[F[_]] {
  import IsEqual.ops._
  import functionals.syntax.all._

  implicit val typeClass: SemigroupK[F]

  def combineAssociativity[A](x: F[A], y: F[A], z: F[A]): IsEqual[F[A]] =
    ((x <+> y) <+> z) =?= (x <+> (y <+> z))
}

object SemigroupKLaws {
  def apply[F[_]: SemigroupK]: SemigroupKLaws[F] = new SemigroupKLaws[F] {
    val typeClass: SemigroupK[F] = SemigroupK[F]
  }
}
