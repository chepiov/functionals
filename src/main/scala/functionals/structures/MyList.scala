package functionals.structures

import functionals.typeclasses._

import scala.annotation.tailrec
import scala.language.higherKinds

sealed trait MyList[+A] { self =>
  import MyList._

  def ::[AA >: A](h: AA): MyList[AA] = MyCons(h, self)

  @tailrec
  final def foldLeft[B](z: B)(f: (B, A) => B): B =
    self match {
      case MyNil        => z
      case MyCons(h, t) => t.foldLeft(f(z, h))(f)
    }

  def reverse: MyList[A] = foldLeft(empty[A])((acc, h) => h :: acc)

  def foldRight[B](z: B)(f: (A, B) => B): B =
    foldLeft[B => B](b => b)((g, a) => b => g(f(a, b)))(z)
  //reverse.foldLeft(z)((b, a) => f(a, b))

  def zip[B](that: MyList[B]): MyList[(A, B)] = {
    val buf = new collection.mutable.ListBuffer[(A, B)]
    @tailrec
    def go(as: MyList[A], bs: MyList[B]): Unit =
      (as, bs) match {
        case (MyNil, _)                       => ()
        case (_, MyNil)                       => ()
        case (MyCons(h1, t1), MyCons(h2, t2)) => buf += Tuple2(h1, h2); go(t1, t2)
      }
    go(self, that)
    MyList(buf: _*)
  }

  def append[AA >: A](that: MyList[AA]): MyList[AA] =
    foldRight(that)((a, as) => MyCons(a, as))

  def ++[AA >: A](that: MyList[AA]): MyList[AA] = append(that)

  def flatMap[B](f: A => MyList[B]): MyList[B] =
    foldRight(empty[B])((a, b) => f(a) ++ b)

  def map[B](f: A => B): MyList[B] =
    foldRight(empty[B]) { case (h, acc) => f(h) :: acc }

  def size: Int = foldLeft(0)((s, _) => s + 1)

  @tailrec
  final def forall(p: A => Boolean): Boolean =
    self match {
      case MyNil        => true
      case MyCons(h, t) => if (p(h)) t.forall(p) else false
    }
}

case object MyNil                                     extends MyList[Nothing]
final case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

case object MyList {

  def apply[A](xs: A*): MyList[A] = {
    (xs foldRight empty[A]) { case (a, acc) => MyCons(a, acc) }
  }

  def empty[A]: MyList[A] = MyNil

  implicit val myListStdInstance: Traverse[MyList] with MonadCombine[MyList] =
    new Traverse[MyList] with MonadCombine[MyList] {
      def traverse[G[_]: Applicative, A, B](fa: MyList[A])(f: A => G[B]): G[MyList[B]] =
        (fa foldRight Applicative[G].pure(empty[B])) { (a, acc) =>
          Applicative[G].map2(f(a), acc)(_ :: _)
        }
      def pure[A](a: A): MyList[A]                                   = a :: MyNil
      def flatMap[A, B](fa: MyList[A])(f: A => MyList[B]): MyList[B] = fa.flatMap(f)
      def foldLeft[A, B](fa: MyList[A], z: B)(f: (B, A) => B): B     = fa.foldLeft(z)(f)
      def empty[A]: MyList[A]                                        = MyNil
      def combine[A](x: MyList[A], y: MyList[A]): MyList[A]          = x ++ y
    }

  implicit def myListMonoid[A: Semigroup]: Monoid[MyList[A]] =
    Monoid.instance(empty[A])((xs, ys) => xs ++ ys)

  implicit def myListEqual[A](implicit A: Equal[A]): Equal[MyList[A]] = new Equal[MyList[A]] {
    def equal(x: MyList[A], y: MyList[A]): Boolean = {
      x.size == y.size && {
        x.zip(y).forall { case (xx, yy) => A.equal(xx, yy) }
      }
    }
  }

  // just for learning
  object PureInstances {

    def myListSemigroup[A]: Semigroup[MyList[A]] = new Semigroup[MyList[A]] {
      def combine(x: MyList[A], y: MyList[A]): MyList[A] = x ++ y
    }

    def myListMonoid[A]: Monoid[MyList[A]] = new Monoid[MyList[A]] {
      def empty: MyList[A]                               = MyNil
      def combine(x: MyList[A], y: MyList[A]): MyList[A] = x ++ y
    }

    val myListFunctor: Functor[MyList] = new Functor[MyList] {
      def map[A, B](fa: MyList[A])(f: A => B): MyList[B] = fa.map(f)
    }

    val myListApplicative: Applicative[MyList] = new Applicative[MyList] {
      def pure[A](a: A): MyList[A] = a :: MyNil
      def ap[A, B](fa: MyList[A])(ff: MyList[A => B]): MyList[B] =
        for {
          a <- fa
          f <- ff
        } yield f(a)
    }

    val myListMonad: Monad[MyList] = new Monad[MyList] {
      def pure[A](a: A): MyList[A]                                   = a :: MyNil
      def flatMap[A, B](fa: MyList[A])(f: A => MyList[B]): MyList[B] = fa.flatMap(f)
    }

    val myListFoldable: Foldable[MyList] = new Foldable[MyList] {
      def foldLeft[A, B](fa: MyList[A], z: B)(f: (B, A) => B): B =
        fa.foldLeft(z)(f)
    }

    val myListTraverse: Traverse[MyList] = new Traverse[MyList] {
      def traverse[G[_]: Applicative, A, B](fa: MyList[A])(f: A => G[B]): G[MyList[B]] =
        (fa foldRight Applicative[G].pure(empty[B])) { (h, t) =>
          Applicative[G].map2(f(h), t)(_ :: _)
        }
      def map[A, B](fa: MyList[A])(f: A => B): MyList[B]         = fa.map(f)
      def foldLeft[A, B](fa: MyList[A], z: B)(f: (B, A) => B): B = fa.foldLeft(z)(f)
    }

    val myListSemigroupK: SemigroupK[MyList] = new SemigroupK[MyList] {
      def combine[A](x: MyList[A], y: MyList[A]): MyList[A] = x ++ y
    }

    val myListMonoidK: MonoidK[MyList] = new MonoidK[MyList] {
      def empty[A]: MyList[A]                               = MyNil
      def combine[A](x: MyList[A], y: MyList[A]): MyList[A] = x ++ y
    }

    val myListMonadFilter: MonadFilter[MyList] = new MonadFilter[MyList] {
      def empty[A]: MyList[A]                                        = MyNil
      def pure[A](a: A): MyList[A]                                   = a :: MyNil
      def flatMap[A, B](fa: MyList[A])(f: A => MyList[B]): MyList[B] = fa flatMap f
    }

    val myListMonadCombine: MonadCombine[MyList] = new MonadCombine[MyList] {
      def empty[A]: MyList[A]                                        = MyNil
      def pure[A](a: A): MyList[A]                                   = a :: MyNil
      def flatMap[A, B](fa: MyList[A])(f: A => MyList[B]): MyList[B] = fa flatMap f
      def combine[A](x: MyList[A], y: MyList[A]): MyList[A]          = x ++ y
    }
  }
}
