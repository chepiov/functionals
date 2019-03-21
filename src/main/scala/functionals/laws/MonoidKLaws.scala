package functionals.laws

import functionals.typeclasses.MonoidK

import scala.language.higherKinds

trait MonoidKLaws[F[_]] extends SemigroupKLaws[F] {

  override implicit val typeClass: MonoidK[F]

  import MonoidK.ops._

  def combineRightIdentity[A](fa: F[A]): Boolean =
    (fa <+> typeClass.empty) == fa

  def combineLeftIdentity[A](fa: F[A]): Boolean =
    (typeClass.empty[A] <+> fa) == fa
}

object MonoidKLaws {
  def apply[F[_]: MonoidK]: MonoidKLaws[F] = new MonoidKLaws[F] {
    val typeClass: MonoidK[F] = MonoidK[F]
  }
}
