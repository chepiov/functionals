package functionals.structures

import functionals.typeclasses.Monad

final case class MyState[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): MyState[S, B] =
    MyState { s =>
      val (a, newS) = run(s)
      (f(a), newS)
    }

  def flatMap[B](f: A => MyState[S, B]): MyState[S, B] =
    MyState { s =>
      val (a, newS) = run(s)
      f(a) run newS
    }
}

case object MyState {

  implicit def myStateMonad[S]: Monad[({ type l[X] = MyState[S, X] })#l] =
    new Monad[({ type l[X] = MyState[S, X] })#l] {
      def flatMap[A, B](fa: MyState[S, A])(f: A => MyState[S, B]): MyState[S, B] =
        fa flatMap f
      def pure[A](a: A): MyState[S, A] = MyState(s => (a, s))
    }
}
