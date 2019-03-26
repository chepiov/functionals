package functionals.typeclasses

import scala.language.higherKinds

trait Applicative[F[_]] extends Any with Apply[F] { self =>

  def pure[A](a: A): F[A]

  def ap2[A, B, Z](fa: F[A], fb: F[B])(ff: F[(A, B) => Z]): F[Z] =
    ap(fa)(ap(fb)(map(ff)(f => b => a => f(a, b))))

  def ap3[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(ff: F[(A, B, C) => Z]): F[Z] =
    ap(fa)(ap(fb)(ap(fc)(map(ff)(f => c => b => a => f(a, b, c)))))

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    ap(fa)(pure(f))

  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    ap(fa)(map(fb)(b => f(_, b)))

  def map3[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => Z): F[Z] =
    ap(fa)(map2(fb, fc)((b, c) => f(_, b, c)))

  def map4[A, B, C, D, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => Z): F[Z] =
    map2(tuple2(fa, fb), tuple2(fc, fd)) { case ((a, b), (c, d)) => f(a, b, c, d) }

  def tuple2[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a, b) => (a, b))

  def tuple3[A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] =
    map3(fa, fb, fc)((a, b, c) => (a, b, c))

  def flip[A, B](fab: F[A => B]): F[A] => F[B] =
    fa => ap(fa)(fab)

  def compose[G[_]](implicit G: Applicative[G]): Applicative[({ type l[X] = F[G[X]] })#l] =
    new Applicative[({ type l[X] = F[G[X]] })#l] {
      def pure[A](a: A): F[G[A]] = self.pure(G.pure(a))
      def ap[A, B](fga: F[G[A]])(ff: F[G[A => B]]): F[G[B]] = {
        val fgab: F[G[A] => G[B]] = self.map(ff)(gab => G.flip(gab))
        self.ap(fga)(fgab)
      }
    }
}

object Applicative {

  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F

  object ops {
    implicit class Syntax[F[_], A](fa: F[A]) extends Functor.ops.Syntax[F, A](fa) {
      def ap[B](ff: F[A => B])(implicit A: Applicative[F]): F[B]  = A.ap(fa)(ff)
      def <*>[B](ff: F[A => B])(implicit A: Applicative[F]): F[B] = ap(ff)
    }
  }
}
