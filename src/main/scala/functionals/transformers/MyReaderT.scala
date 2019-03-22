package functionals.transformers
import functionals.typeclasses.{Applicative, Equal, Functor, Monad}

final case class MyReaderT[F[_], A, B](run: A => F[B]) {

  def map[C](f: B => C)(implicit F: Functor[F]): MyReaderT[F, A, C] =
    MyReaderT { a =>
      F.map(run(a))(f)
    }

  def flatMap[C](f: B => MyReaderT[F, A, C])(implicit F: Monad[F]): MyReaderT[F, A, C] =
    MyReaderT { a =>
      F.flatMap(run(a)) { b =>
        f(b).run(a)
      }
    }

  def andThen[C](that: MyReaderT[F, B, C])(implicit F: Monad[F]): MyReaderT[F, A, C] =
    MyReaderT { a =>
      F.flatMap(run(a)) { b =>
        that.run(b)
      }
    }

  def compose[C](that: MyReaderT[F, C, A])(implicit F: Monad[F]): MyReaderT[F, C, B] =
    that andThen this
}

case object MyReaderT {

  def ask[F[_], A](implicit F: Applicative[F]): MyReaderT[F, A, A] =
    MyReaderT { a =>
      F.pure(a)
    }

  implicit def myReaderTMonad[F[_], R](implicit F: Monad[F]): Monad[({ type l[X] = MyReaderT[F, R, X] })#l] =
    new Monad[({ type l[X] = MyReaderT[F, R, X] })#l] {
      def pure[A](a: A): MyReaderT[F, R, A] = MyReaderT(_ => F.pure(a))
      def flatMap[A, B](fa: MyReaderT[F, R, A])(f: A => MyReaderT[F, R, B]): MyReaderT[F, R, B] =
        fa.flatMap(f)
    }

  implicit def myReaderT[F[_], A, B]: Equal[MyReaderT[F, A, B]] = new Equal[MyReaderT[F, A, B]] {
    def equal(x: MyReaderT[F, A, B], y: MyReaderT[F, A, B]): Boolean = x.run == y.run
  }
}
