package functionals.transformers

import functionals.structures.{MyNone, MyOption, MySome}
import functionals.typeclasses.{Equal, Functor, Monad}

import scala.language.higherKinds

final case class MyOptionT[F[_], A](value: F[MyOption[A]]) {

  def map[B](f: A => B)(implicit F: Functor[F]): MyOptionT[F, B] =
    MyOptionT(F.map(value)(optA => optA map f))

  def flatMap[B](f: A => MyOptionT[F, B])(implicit F: Monad[F]): MyOptionT[F, B] =
    MyOptionT(
      F.flatMap(value)(
        optA =>
          optA.map { a =>
            f(a).value
          }.getOrElse(F.pure(MyNone))
      )
    )

  def flatMapF[B](f: A => F[MyOption[B]])(implicit F: Monad[F]): MyOptionT[F, B] =
    flatMap(f andThen MyOptionT.apply)

  def isDefined(implicit F: Functor[F]): F[Boolean]          = F.map(value)(_.isDefined)
  def isEmpty(implicit F: Functor[F]): F[Boolean]            = F.map(value)(_.isEmpty)
  def getOrElse(default: => A)(implicit F: Functor[F]): F[A] = F.map(value)(_.getOrElse(default))
}

case object MyOptionT {

  implicit def myOptionTMonad[F[_]](implicit F: Monad[F]): Monad[({ type l[X] = MyOptionT[F, X] })#l] =
    new Monad[({ type l[X] = MyOptionT[F, X] })#l] {
      def pure[A](a: A): MyOptionT[F, A] = MyOptionT(F.pure(MySome(a)))
      def flatMap[A, B](fa: MyOptionT[F, A])(f: A => MyOptionT[F, B]): MyOptionT[F, B] =
        fa.flatMap(f)
    }

  implicit def myOptionTEqual[F[_], A](implicit F: Equal[F[MyOption[A]]]): Equal[MyOptionT[F, A]] =
    new Equal[MyOptionT[F, A]] {
      def equal(x: MyOptionT[F, A], y: MyOptionT[F, A]): Boolean =
        F.equal(x.value, y.value)
    }
}
