package functionals.transformers

import functionals.structures.{MyEither, MyLeft, MyRight}
import functionals.typeclasses._

final case class MyEitherT[F[_], L, R](value: F[MyEither[L, R]]) {

  def map[S](f: R => S)(implicit F: Functor[F]): MyEitherT[F, L, S] =
    MyEitherT(F.map(value)(e => e.map(f)))

  def flatMap[S](f: R => MyEitherT[F, L, S])(implicit F: Monad[F]): MyEitherT[F, L, S] =
    MyEitherT(
      F.flatMap(value) {
        case MyRight(r) => f(r).value
        case MyLeft(l)  => F.pure(MyLeft(l))
      }
    )

  def +++(that: MyEitherT[F, L, R])(
      implicit M1: Semigroup[R],
      M2: Semigroup[L],
      F: Applicative[F]
  ): MyEitherT[F, L, R] =
    MyEitherT(F.map2(value, that.value)(_ +++ _))
}

case object MyEitherT {

  implicit def myEitherTMonad[F[_], L](implicit F: Monad[F]): Monad[({ type l[X] = MyEitherT[F, L, X] })#l] =
    new Monad[({ type l[X] = MyEitherT[F, L, X] })#l] {
      def pure[A](a: A): MyEitherT[F, L, A] = MyEitherT(F.pure(MyRight(a)))
      def flatMap[A, B](fa: MyEitherT[F, L, A])(f: A => MyEitherT[F, L, B]): MyEitherT[F, L, B] =
        fa.flatMap(f)
    }

  implicit def myEitherTEqual[F[_], L, R](implicit F: Equal[F[MyEither[L, R]]]): Equal[MyEitherT[F, L, R]] =
    new Equal[MyEitherT[F, L, R]] {
      def equal(x: MyEitherT[F, L, R], y: MyEitherT[F, L, R]): Boolean =
        F.equal(x.value, y.value)
    }
}
