package functionals.laws

import functionals.typeclasses.MonoidK

import scala.language.higherKinds

trait MonoidKLaws[F[_]] extends SemigroupKLaws[F] {
  import IsEqual.ops._
  import MonoidK.ops._

  override implicit val typeClass: MonoidK[F]

  def combineRightIdentity[A](fa: F[A]): IsEqual[F[A]] =
    (fa <+> typeClass.empty) =?= fa

  def combineLeftIdentity[A](fa: F[A]): IsEqual[F[A]] =
    (typeClass.empty[A] <+> fa) =?= fa
}

object MonoidKLaws {
  def apply[F[_]: MonoidK]: MonoidKLaws[F] = new MonoidKLaws[F] {
    val typeClass: MonoidK[F] = MonoidK[F]
  }
}
