package functionals.laws

import functionals.typeclasses.MonadCombine

import scala.language.higherKinds

trait MonadCombineLaws[F[_]] extends MonadFilterLaws[F] with MonoidKLaws[F] {
  //import IsEqual.ops._
  //import MonadCombine.ops._

  implicit val typeClass: MonadCombine[F]

  // see: https://github.com/typelevel/cats/issues/693
  // the left distributivity law does not hold for things like
  // MonadCombine[Option]; here's a counter-example:
  //
  //     def f(x: Int): Option[Int] = if (x == 0) None else Some(x)
  //     val fa = Option(0)
  //     val fa2 = Option(1)
  //     (a <+> b).flatMap(f) != (a.flatMap(f) <+> b.flatMap(f))
  //
  // def monadCombineLeftDistributivity[A, B](fa: F[A], fa2: F[A], f: A => F[B]): IsEqual[F[B]] =
  //   (fa <+> fa2).flatMap(f) =?= ((fa flatMap f) <+> (fa2 flatMap f))
}

object MonadCombineLaws {
  def apply[F[_]: MonadCombine]: MonadCombineLaws[F] = new MonadCombineLaws[F] {
    val typeClass: MonadCombine[F] = MonadCombine[F]
  }
}
