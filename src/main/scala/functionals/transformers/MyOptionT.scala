package functionals.transformers

import functionals.structures.{MyNone, MyOption, MySome}
import functionals.typeclasses.{Equal, Monad}

import scala.language.{higherKinds, reflectiveCalls}

final case class MyOptionT[F[_], A](value: F[MyOption[A]])

case object MyOptionT {

  implicit def optionTMonad[F[_]](implicit F: Monad[F]): Monad[({ type l[X] = MyOptionT[F, X] })#l] =
    new Monad[({ type l[X] = MyOptionT[F, X] })#l] {
      def pure[A](a: A): MyOptionT[F, A] = MyOptionT(F.pure(MySome(a)))
      def flatMap[A, B](fa: MyOptionT[F, A])(f: A => MyOptionT[F, B]): MyOptionT[F, B] =
        MyOptionT(F.flatMap(fa.value)(optA =>
          optA.map { a =>
            f(a).value
          }.getOrElse(F.pure(MyNone))))
    }

  implicit def myOptionTEqual[F[_], A](implicit F: Equal[F[MyOption[A]]]): Equal[MyOptionT[F, A]] =
    new Equal[MyOptionT[F, A]] {
      def equal(x: MyOptionT[F, A], y: MyOptionT[F, A]): Boolean =
        F.equal(x.value, y.value)
    }
}
