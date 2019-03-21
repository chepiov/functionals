package functionals.structures

import functionals.typeclasses._

import scala.language.higherKinds

sealed trait MyOption[+A] { self =>
  import MyOption._

  def fold[B](z: B)(f: A => B): B = self match {
    case MyNone    => z
    case MySome(a) => f(a)
  }

  def map[B](f: A => B): MyOption[B] =
    fold(none[B])(a => some(f(a)))

  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
    fold(none[B])(a => f(a))

  def orElse[AA >: A](that: MyOption[AA]): MyOption[AA] = fold(that)(_ => self)

  def getOrElse[AA >: A](default: AA): AA = fold(default)(identity)

  def isEmpty: Boolean   = fold(true)(_ => false)
  def isDefined: Boolean = fold(false)(_ => true)
}

case object MyNone                    extends MyOption[Nothing]
final case class MySome[+A](value: A) extends MyOption[A]

case object MyOption {

  def some[A](a: A): MyOption[A] = MySome(a)

  def none[A]: MyOption[A] = MyNone

  def apply[A](a: A): MyOption[A] = some(a)

  implicit val myOptionStdInstance: Traverse[MyOption] with MonadCombine[MyOption] =
    new Traverse[MyOption] with MonadCombine[MyOption] {
      def traverse[G[_]: Applicative, A, B](fa: MyOption[A])(f: A => G[B]): G[MyOption[B]] =
        fa match {
          case MySome(a) => Applicative[G].map(f(a))(MySome(_))
          case MyNone    => Applicative[G].pure(MyNone)
        }
      def pure[A](a: A): MyOption[A]                                       = some(a)
      def flatMap[A, B](fa: MyOption[A])(f: A => MyOption[B]): MyOption[B] = fa.flatMap(f)
      def foldLeft[A, B](fa: MyOption[A], z: B)(f: (B, A) => B): B         = fa.fold(z)(a => f(z, a))
      def empty[A]: MyOption[A]                                            = none[A]
      def combine[A](x: MyOption[A], y: MyOption[A]): MyOption[A]          = x orElse y
    }

  implicit def myOptionMonoid[A](implicit S: Semigroup[A]): Monoid[MyOption[A]] =
    new Monoid[MyOption[A]] {
      def empty: MyOption[A] = MyNone
      def combine(x: MyOption[A], y: MyOption[A]): MyOption[A] =
        (x, y) match {
          case (MyNone, MyNone)         => none
          case (MySome(xx), MyNone)     => some(xx)
          case (MyNone, MySome(yy))     => some(yy)
          case (MySome(xx), MySome(yy)) => some(S.combine(xx, yy))
        }
    }

  implicit def myOptionEqual[A](implicit A: Equal[A]): Equal[MyOption[A]] = new Equal[MyOption[A]] {
    def equal(x: MyOption[A], y: MyOption[A]): Boolean =
      (x, y) match {
        case (MySome(a), MySome(b)) => A.equal(a, b)
        case (MyNone, MyNone)       => true
        case _                      => false
      }
  }

  // just for learning
  object PureInstances {

    def myOptionSemigroup[A: Semigroup]: Semigroup[MyOption[A]] = new Semigroup[MyOption[A]] {
      def combine(x: MyOption[A], y: MyOption[A]): MyOption[A] =
        (x, y) match {
          case (MySome(a1), MySome(a2)) => some(Semigroup[A].combine(a1, a2))
          case (MySome(_), _)           => x
          case (_, MySome(_))           => y
          case _                        => none
        }
    }

    def myOptionMonoid[A: Semigroup]: Monoid[MyOption[A]] = new Monoid[MyOption[A]] {
      def empty: MyOption[A] = MyNone
      def combine(x: MyOption[A], y: MyOption[A]): MyOption[A] =
        (x, y) match {
          case (MySome(a1), MySome(a2)) => some(Semigroup[A].combine(a1, a2))
          case (MySome(_), _)           => x
          case (_, MySome(_))           => y
          case _                        => none
        }
    }

    val myOptionFunctor: Functor[MyOption] = new Functor[MyOption] {
      def map[A, B](fa: MyOption[A])(f: A => B): MyOption[B] = fa.map(f)
    }

    val myOptionApplicative: Applicative[MyOption] = new Applicative[MyOption] {
      def pure[A](a: A): MyOption[A] = some(a)
      def ap[A, B](fa: MyOption[A])(ff: MyOption[A => B]): MyOption[B] =
        (fa, ff) match {
          case (MySome(a), MySome(f)) => some(f(a))
          case _                      => none
        }
    }

    val myOptionMonad: Monad[MyOption] = new Monad[MyOption] {
      def pure[A](a: A): MyOption[A]                                       = some(a)
      def flatMap[A, B](fa: MyOption[A])(f: A => MyOption[B]): MyOption[B] = fa.flatMap(f)
    }

    val myOptionFoldable: Foldable[MyOption] = new Foldable[MyOption] {
      def foldLeft[A, B](fa: MyOption[A], z: B)(f: (B, A) => B): B = fa.fold(z)(a => f(z, a))
    }

    val myOptionTraverse: Traverse[MyOption] = new Traverse[MyOption] {
      def foldLeft[A, B](fa: MyOption[A], z: B)(f: (B, A) => B): B = fa.fold(z)(a => f(z, a))
      def map[A, B](fa: MyOption[A])(f: A => B): MyOption[B]       = fa.map(f)
      def traverse[G[_]: Applicative, A, B](fa: MyOption[A])(f: A => G[B]): G[MyOption[B]] =
        fa match {
          case MyNone    => Applicative[G].pure(none)
          case MySome(a) => Applicative[G].map(f(a))(some)
        }
    }

    def semigroup[A: Semigroup]: Semigroup[MyOption[A]] = new Semigroup[MyOption[A]] {
      def combine(x: MyOption[A], y: MyOption[A]): MyOption[A] =
        (x, y) match {
          case (MySome(a), MySome(b)) => MySome(Semigroup[A].combine(a, b))
          case (MySome(_), MyNone)    => x
          case (MyNone, MySome(_))    => y
          case _                      => MyNone
        }
    }

    def monoid[A: Semigroup]: Monoid[MyOption[A]] = new Monoid[MyOption[A]] {
      def empty: MyOption[A] = MyNone
      def combine(x: MyOption[A], y: MyOption[A]): MyOption[A] =
        (x, y) match {
          case (MySome(a), MySome(b)) => MySome(Semigroup[A].combine(a, b))
          case (MySome(_), MyNone)    => x
          case (MyNone, MySome(_))    => y
          case _                      => MyNone
        }
    }

    val myOptionSemigroupK: SemigroupK[MyOption] = new SemigroupK[MyOption] {
      def combine[A](x: MyOption[A], y: MyOption[A]): MyOption[A] = x orElse y
    }

    val myOptionMonoidK: MonoidK[MyOption] = new MonoidK[MyOption] {
      def combine[A](x: MyOption[A], y: MyOption[A]): MyOption[A] = x orElse y
      def empty[A]: MyOption[A]                                   = none
    }

    val myOptionMonadFilter: MonadFilter[MyOption] = new MonadFilter[MyOption] {
      def empty[A]: MyOption[A]                                            = none[A]
      def pure[A](a: A): MyOption[A]                                       = some(a)
      def flatMap[A, B](fa: MyOption[A])(f: A => MyOption[B]): MyOption[B] = fa flatMap f
    }

    val myOptionMonadCombine: MonadCombine[MyOption] = new MonadCombine[MyOption] {
      def empty[A]: MyOption[A]                                            = none[A]
      def combine[A](x: MyOption[A], y: MyOption[A]): MyOption[A]          = x orElse y
      def pure[A](a: A): MyOption[A]                                       = some(a)
      def flatMap[A, B](fa: MyOption[A])(f: A => MyOption[B]): MyOption[B] = fa flatMap f
    }
  }
}
