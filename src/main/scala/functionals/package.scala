import functionals.typeclasses._

import scala.language.higherKinds

package object functionals {
  type MyId[A] = A

  implicit val idStdInstance: Traverse[MyId] with Monad[MyId] =
    new Traverse[MyId] with Monad[MyId] {
      def traverse[G[_]: Applicative, A, B](fa: MyId[A])(f: A => G[B]): G[MyId[B]] = f(fa)
      def pure[A](a: A): MyId[A]                                                   = a
      def flatMap[A, B](fa: MyId[A])(f: A => MyId[B]): MyId[B]                     = f(fa)
      def foldLeft[A, B](fa: MyId[A], z: B)(f: (B, A) => B): B                     = f(z, fa)
    }

  implicit val byteMonoid: Monoid[Byte]     = Monoid.instance[Byte](0)((x, y) => (x + y).toByte)
  implicit val intMonoid: Monoid[Int]       = Monoid.instance[Int](0)((x, y) => x + y)
  implicit val longMonoid: Monoid[Long]     = Monoid.instance[Long](0L)((x, y) => x + y)
  implicit val shortMonoid: Monoid[Short]   = Monoid.instance[Short](0)((x, y) => (x + y).toShort)
  implicit val stringMonoid: Monoid[String] = Monoid.instance[String]("")((x, y) => x + y)

  implicit val byteEqual: Equal[Byte]       = Equal.natural
  implicit val intEqual: Equal[Int]         = Equal.natural
  implicit val longEqual: Equal[Long]       = Equal.natural
  implicit val shortEqual: Equal[Short]     = Equal.natural
  implicit val stringEqual: Equal[String]   = Equal.natural
  implicit val booleanEqual: Equal[Boolean] = Equal.natural
  implicit val bigIntEqual: Equal[BigInt]   = Equal.natural

  // just for learning
  object PureInstances {

    val myIdFunctor: Functor[MyId] = new Functor[MyId] {
      def map[A, B](fa: MyId[A])(f: A => B): MyId[B] = f(fa)
    }

    val myIdApplicative: Applicative[MyId] = new Applicative[MyId] {
      def pure[A](a: A): MyId[A]                          = a
      def ap[A, B](fa: MyId[A])(f: MyId[A => B]): MyId[B] = f(fa)
    }

    val myIdMonad: Monad[MyId] = new Monad[MyId] {
      def pure[A](a: A): MyId[A]                               = a
      def flatMap[A, B](fa: MyId[A])(f: A => MyId[B]): MyId[B] = f(fa)
    }

    val myIdFoldable: Foldable[MyId] = new Foldable[MyId] {
      def foldLeft[A, B](fa: MyId[A], z: B)(f: (B, A) => B): B = f(z, fa)
    }

    val myIdTraverse: Traverse[MyId] = new Traverse[MyId] {
      def map[A, B](fa: MyId[A])(f: A => B): MyId[B]                               = f(fa)
      def foldLeft[A, B](fa: MyId[A], z: B)(f: (B, A) => B): B                     = f(z, fa)
      def traverse[G[_]: Applicative, A, B](fa: MyId[A])(f: A => G[B]): G[MyId[B]] = f(fa)
    }
  }
}
