package functionals.structures

import functionals.typeclasses.Monad

case class MyReader[A, B](run: A => B) {

  def map[C](f: B => C): MyReader[A, C] =
    MyReader(run andThen f)

  def flatMap[C](f: B => MyReader[A, C]): MyReader[A, C] =
    MyReader { a =>
      f(run(a)).run(a)
    }

  def andThen[C](that: MyReader[B, C]): MyReader[A, C] =
    MyReader(run andThen that.run)

  def compose[C](that: MyReader[C, A]): MyReader[C, B] =
    MyReader(that.run andThen run)
}

case object MyReader {

  def ask[A]: MyReader[A, A] = MyReader(identity)

  implicit def myReaderMonad[R]: Monad[({ type l[X] = MyReader[R, X] })#l] =
    new Monad[({ type l[X] = MyReader[R, X] })#l] {
      def pure[A](a: A): MyReader[R, A] = MyReader(_ => a)
      def flatMap[A, B](fa: MyReader[R, A])(f: A => MyReader[R, B]): MyReader[R, B] =
        fa.flatMap(f)
    }
}
