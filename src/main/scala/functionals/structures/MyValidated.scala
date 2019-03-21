package functionals.structures

import functionals.typeclasses._

import scala.language.{higherKinds, reflectiveCalls}

sealed trait MyValidated[+E, +A] { self =>
  import MyValidated._

  def fold[B](fe: E => B)(fa: A => B): B = self match {
    case MyValid(a)   => fa(a)
    case MyInvalid(e) => fe(e)
  }

  def map[B](f: A => B): MyValidated[E, B] =
    fold(invalid[E, B])(a => valid(f(a)))

  def isValid: Boolean = self match {
    case MyValid(_) => true
    case _          => false
  }

  def isInvalid: Boolean = self match {
    case MyInvalid(_) => true
    case _            => false
  }

  def toOption: MyOption[A] = self match {
    case MyValid(a) => MyOption.some(a)
    case _          => MyOption.none
  }

  def toEither: MyEither[E, A] = self match {
    case MyValid(a)   => MyEither.right(a)
    case MyInvalid(e) => MyEither.left(e)
  }
}

final case class MyValid[+A](a: A)   extends MyValidated[Nothing, A]
final case class MyInvalid[+E](e: E) extends MyValidated[E, Nothing]

case object MyValidated {

  def valid[E, A](a: A): MyValidated[E, A] = MyValid(a)

  def invalid[E, A](e: E): MyValidated[E, A] = MyInvalid(e)

  implicit def myValidatedStdInstances[E: Semigroup]
    : Traverse[({ type l[X] = MyValidated[E, X] })#l] with Applicative[({ type l[X] = MyValidated[E, X] })#l] =
    new Traverse[({ type l[X] = MyValidated[E, X] })#l] with Applicative[({ type l[X] = MyValidated[E, X] })#l] {
      def pure[A](a: A): MyValidated[E, A] = valid(a)
      def ap[A, B](fa: MyValidated[E, A])(ff: MyValidated[E, A => B]): MyValidated[E, B] =
        (fa, ff) match {
          case (MyValid(a), MyValid(f))       => valid(f(a))
          case (MyInvalid(e1), MyInvalid(e2)) => invalid(Semigroup[E].combine(e1, e2))
          case (e @ MyInvalid(_), _)          => e
          case (_, e @ MyInvalid(_))          => e
        }
      def traverse[G[_]: Applicative, A, B](fa: MyValidated[E, A])(f: A => G[B]): G[MyValidated[E, B]] =
        fa match {
          case MyValid(a)       => Applicative[G].map(f(a))(valid)
          case e @ MyInvalid(_) => Applicative[G].pure(e)
        }
      def foldLeft[A, B](fa: MyValidated[E, A], z: B)(f: (B, A) => B): B = fa.fold(_ => z)(a => f(z, a))
    }

  implicit def myValidatedMonoid[E: Semigroup, A: Monoid]: Monoid[({ type l = MyValidated[E, A] })#l] =
    Monoid.instance(valid[E, A](Monoid[A].empty)) { (these, that) =>
      (these, that) match {
        case (MyValid(a1), MyValid(a2))     => valid(Monoid[A].combine(a1, a2))
        case (MyInvalid(e1), MyInvalid(e2)) => invalid(Semigroup[E].combine(e1, e2))
        case (MyInvalid(_), _)              => these
        case _                              => that
      }
    }

  implicit def myValidatedSemigroupK[E]: SemigroupK[({ type l[X] = MyValidated[E, X] })#l] =
    new SemigroupK[({ type l[X] = MyValidated[E, X] })#l] {
      def combine[A](x: MyValidated[E, A], y: MyValidated[E, A]): MyValidated[E, A] =
        x match {
          case MyInvalid(_) => y
          case MyValid(_)   => x
        }
    }

  implicit def myValidatedEqual[E, A](implicit E: Equal[E], A: Equal[A]): Equal[MyValidated[E, A]] =
    new Equal[MyValidated[E, A]] {
      def equal(x: MyValidated[E, A], y: MyValidated[E, A]): Boolean =
        (x, y) match {
          case (MyValid(a), MyValid(b))     => A.equal(a, b)
          case (MyInvalid(a), MyInvalid(b)) => E.equal(a, b)
          case _                            => false
        }
    }

  // just for learning
  object PureInstances {

    def myValidatedSemigroup[E: Semigroup, A: Semigroup]: Semigroup[MyValidated[E, A]] =
      new Semigroup[MyValidated[E, A]] {
        def combine(x: MyValidated[E, A], y: MyValidated[E, A]): MyValidated[E, A] =
          (x, y) match {
            case (MyValid(a1), MyValid(a2))     => MyValid(Semigroup[A].combine(a1, a2))
            case (MyInvalid(e1), MyInvalid(e2)) => MyInvalid(Semigroup[E].combine(e1, e2))
            case (MyInvalid(_), _)              => x
            case (_, MyInvalid(_))              => y
          }
      }

    def myValidatedMonoid[E: Semigroup, A: Monoid]: Monoid[MyValidated[E, A]] =
      new Monoid[MyValidated[E, A]] {
        def empty: MyValidated[E, A] = valid(Monoid[A].empty)
        def combine(x: MyValidated[E, A], y: MyValidated[E, A]): MyValidated[E, A] =
          (x, y) match {
            case (MyValid(a1), MyValid(a2))     => MyValid(Semigroup[A].combine(a1, a2))
            case (MyInvalid(e1), MyInvalid(e2)) => MyInvalid(Semigroup[E].combine(e1, e2))
            case (MyInvalid(_), _)              => x
            case (_, MyInvalid(_))              => y
          }
      }

    def myValidatedFunctor[E]: Functor[({ type l[X] = MyValidated[E, X] })#l] =
      new Functor[({ type l[X] = MyValidated[E, X] })#l] {
        def map[A, B](fa: MyValidated[E, A])(f: A => B): MyValidated[E, B] = fa map f
      }

    def myValidatedApplicative[E: Semigroup]: Applicative[({ type l[X] = MyValidated[E, X] })#l] =
      new Applicative[({ type l[X] = MyValidated[E, X] })#l] {
        def pure[A](a: A): MyValidated[E, A] = valid(a)
        def ap[A, B](fa: MyValidated[E, A])(ff: MyValidated[E, A => B]): MyValidated[E, B] =
          (fa, ff) match {
            case (MyValid(a), MyValid(f))       => valid(f(a))
            case (MyInvalid(e1), MyInvalid(e2)) => invalid(Semigroup[E].combine(e1, e2))
            case (e @ MyInvalid(_), _)          => e
            case (_, e @ MyInvalid(_))          => e
          }
      }

    def myValidatedFoldable[E]: Foldable[({ type l[X] = MyValidated[E, X] })#l] =
      new Foldable[({ type l[X] = MyValidated[E, X] })#l] {
        def foldLeft[A, B](fa: MyValidated[E, A], z: B)(f: (B, A) => B): B = fa.fold(_ => z)(a => f(z, a))
      }

    def myValidatedTraverse[E]: Traverse[({ type l[X] = MyValidated[E, X] })#l] =
      new Traverse[({ type l[X] = MyValidated[E, X] })#l] {
        def map[A, B](fa: MyValidated[E, A])(f: A => B): MyValidated[E, B] = fa map f
        def foldLeft[A, B](fa: MyValidated[E, A], z: B)(f: (B, A) => B): B = fa.fold(_ => z)(a => f(z, a))
        def traverse[G[_]: Applicative, A, B](fa: MyValidated[E, A])(f: A => G[B]): G[MyValidated[E, B]] =
          fa match {
            case MyValid(a)       => Applicative[G].map(f(a))(valid)
            case e @ MyInvalid(_) => Applicative[G].pure(e)
          }
      }

    def myValidatedSemigroupK[E]: SemigroupK[({ type l[X] = MyValidated[E, X] })#l] =
      new SemigroupK[({ type l[X] = MyValidated[E, X] })#l] {
        def combine[A](x: MyValidated[E, A], y: MyValidated[E, A]): MyValidated[E, A] =
          x match {
            case MyInvalid(_) => y
            case MyValid(_)   => x
          }
      }
  }
}
