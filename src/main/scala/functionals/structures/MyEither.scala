package functionals.structures

import functionals.typeclasses._

import scala.language.{higherKinds, reflectiveCalls}

sealed trait MyEither[+E, +A] { self =>
  import MyEither._

  def fold[B](l: E => B)(r: A => B): B =
    self match {
      case MyLeft(e)  => l(e)
      case MyRight(a) => r(a)

    }
  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] =
    fold[MyEither[EE, B]](left)(f)

  def leftFlatMap[EE, AA >: A](f: E => MyEither[EE, AA]): MyEither[EE, AA] =
    fold[MyEither[EE, AA]](f)(right)

  def map[B](f: A => B): MyEither[E, B] =
    fold[MyEither[E, B]](left)(right[E, B] _ compose f)

  def leftMap[EE](f: E => EE): MyEither[EE, A] =
    fold[MyEither[EE, A]](left[EE, A] _ compose f)(right)

  def toOption: MyOption[A] =
    fold[MyOption[A]](_ => MyOption.none[A])(MyOption.some)

  def swap: MyEither[A, E] =
    fold[MyEither[A, E]](right)(left)
}

final case class MyLeft[+E](e: E)  extends MyEither[E, Nothing]
final case class MyRight[+A](a: A) extends MyEither[Nothing, A]

case object MyEither {

  def apply[E, A](a: A): MyEither[E, A] = MyRight(a)

  def left[E, A](e: E): MyEither[E, A] = MyLeft(e)

  def right[E, A](a: A): MyEither[E, A] = MyRight(a)

  implicit def myEitherStdInstance[E]
      : Traverse[({ type l[X] = MyEither[E, X] })#l] with Monad[({ type l[X] = MyEither[E, X] })#l] =
    new Traverse[({ type l[X] = MyEither[E, X] })#l] with Monad[({ type l[X] = MyEither[E, X] })#l] {
      def traverse[G[_]: Applicative, A, B](fa: MyEither[E, A])(f: A => G[B]): G[MyEither[E, B]] =
        fa match {
          case MyLeft(e)  => Applicative[G].pure(left(e))
          case MyRight(a) => Applicative[G].map(f(a))(right)
        }
      def pure[A](a: A): MyEither[E, A]                                             = right(a)
      def flatMap[A, B](fa: MyEither[E, A])(f: A => MyEither[E, B]): MyEither[E, B] = fa.flatMap(f)
      def foldLeft[A, B](fa: MyEither[E, A], z: B)(f: (B, A) => B): B               = fa.fold(_ => z)(a => f(z, a))
    }

  implicit def myEitherMonoid[E, A](implicit M: Monoid[A]): Monoid[MyEither[E, A]] =
    Monoid.instance[MyEither[E, A]](right(M.empty)) {
      case (l @ MyLeft(_), _)       => l
      case (_, r @ MyLeft(_))       => r
      case (MyRight(x), MyRight(y)) => right(M.combine(x, y))
    }

  implicit def myEitherSemigroupK[E]: SemigroupK[({ type l[X] = MyEither[E, X] })#l] =
    new SemigroupK[({ type l[X] = MyEither[E, X] })#l] {
      def combine[A](x: MyEither[E, A], y: MyEither[E, A]): MyEither[E, A] = x match {
        case MyLeft(_)  => y
        case MyRight(_) => x
      }
    }

  implicit def myEitherEqual[E, A](implicit E: Equal[E], A: Equal[A]): Equal[MyEither[E, A]] =
    new Equal[MyEither[E, A]] {
      def equal(x: MyEither[E, A], y: MyEither[E, A]): Boolean = (x, y) match {
        case (MyRight(a), MyRight(b)) => A.equal(a, b)
        case (MyLeft(a), MyLeft(b))   => E.equal(a, b)
        case _                        => false
      }
    }

  // just for learning
  object PureInstances {

    def myEitherSemigroup[E, A: Monoid]: Semigroup[MyEither[E, A]] =
      new Semigroup[MyEither[E, A]] {
        def combine(x: MyEither[E, A], y: MyEither[E, A]): MyEither[E, A] =
          (x, y) match {
            case (l @ MyLeft(_), _)         => l
            case (_, r @ MyLeft(_))         => r
            case (MyRight(a1), MyRight(a2)) => right(Monoid[A].combine(a1, a2))
          }
      }

    def myEitherMonoid[E, A: Monoid]: Monoid[MyEither[E, A]] =
      new Monoid[MyEither[E, A]] {
        def empty: MyEither[E, A] = right(Monoid[A].empty)
        def combine(x: MyEither[E, A], y: MyEither[E, A]): MyEither[E, A] =
          (x, y) match {
            case (l @ MyLeft(_), _)         => l
            case (_, r @ MyLeft(_))         => r
            case (MyRight(a1), MyRight(a2)) => right(Monoid[A].combine(a1, a2))
          }
      }

    def myEitherFunctor[E]: Functor[({ type l[X] = MyEither[E, X] })#l] =
      new Functor[({ type l[X] = MyEither[E, X] })#l] {
        def map[A, B](fa: MyEither[E, A])(f: A => B): MyEither[E, B] = fa.map(f)
      }

    def myEitherApplicative[E]: Applicative[({ type l[X] = MyEither[E, X] })#l] =
      new Applicative[({ type l[X] = MyEither[E, X] })#l] {
        def pure[A](a: A): MyEither[E, A] = right(a)
        def ap[A, B](fa: MyEither[E, A])(ff: MyEither[E, A => B]): MyEither[E, B] =
          ff match {
            case MyRight(f) => fa map f
            case MyLeft(e)  => left(e)
          }
      }

    def myEitherMonad[E]: Monad[({ type l[X] = MyEither[E, X] })#l] =
      new Monad[({ type l[X] = MyEither[E, X] })#l] {
        def pure[A](a: A): MyEither[E, A] = right(a)
        def flatMap[A, B](fa: MyEither[E, A])(f: A => MyEither[E, B]): MyEither[E, B] =
          fa.flatMap(f)
      }

    def myEitherFoldable[E]: Foldable[({ type l[X] = MyEither[E, X] })#l] =
      new Foldable[({ type l[X] = MyEither[E, X] })#l] {
        def foldLeft[A, B](fa: MyEither[E, A], z: B)(f: (B, A) => B): B = fa.fold(_ => z)(a => f(z, a))
      }

    def myEitherTraverse[E]: Traverse[({ type l[X] = MyEither[E, X] })#l] =
      new Traverse[({ type l[X] = MyEither[E, X] })#l] {
        def map[A, B](fa: MyEither[E, A])(f: A => B): MyEither[E, B]    = fa.map(f)
        def foldLeft[A, B](fa: MyEither[E, A], z: B)(f: (B, A) => B): B = fa.fold(_ => z)(a => f(z, a))
        def traverse[G[_]: Applicative, A, B](fa: MyEither[E, A])(f: A => G[B]): G[MyEither[E, B]] =
          fa match {
            case MyRight(a)    => Applicative[G].map(f(a))(right)
            case l @ MyLeft(_) => Applicative[G].pure(l)
          }
      }

    def semigroupK[E]: SemigroupK[({ type l[X] = MyEither[E, X] })#l] =
      new SemigroupK[({ type l[X] = MyEither[E, X] })#l] {
        def combine[A](x: MyEither[E, A], y: MyEither[E, A]): MyEither[E, A] = x match {
          case MyLeft(_)  => y
          case MyRight(_) => x
        }
      }
  }
}
