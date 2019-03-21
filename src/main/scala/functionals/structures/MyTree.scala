package functionals.structures

import functionals.typeclasses._

import scala.language.higherKinds

sealed trait MyTree[+A] { self =>
  import MyTree._

  def fold[B](lf: A => B)(nf: (B, B) => B): B =
    self match {
      case Leaf(a)           => lf(a)
      case Node(left, right) => nf(left.fold(lf)(nf), right.fold(lf)(nf))
    }

  def flatMap[B](f: A => MyTree[B]): MyTree[B] =
    fold[MyTree[B]](f)((l, f) => node(l, f))

  def map[B](f: A => B): MyTree[B] =
    flatMap(a => leaf(f(a)))

  def size: Int = fold(_ => 1)((a, b) => a + b + 1)
}

final case class Leaf[+A](a: A)                              extends MyTree[A]
final case class Node[+A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

case object MyTree {

  def leaf[A](a: A): MyTree[A] = Leaf(a)

  def node[A](left: MyTree[A], right: MyTree[A]): MyTree[A] = Node(left, right)

  implicit val myTreeStdInstances: Traverse[MyTree] with Monad[MyTree] =
    new Traverse[MyTree] with Monad[MyTree] {
      def traverse[G[_]: Applicative, A, B](fa: MyTree[A])(f: A => G[B]): G[MyTree[B]] =
        fa match {
          case Node(l, r) => Applicative[G].map2(traverse(l)(f), traverse(r)(f))(node)
          case Leaf(a)    => Applicative[G].map(f(a))(leaf)
        }
      def pure[A](a: A): MyTree[A]                                   = leaf(a)
      def flatMap[A, B](fa: MyTree[A])(f: A => MyTree[B]): MyTree[B] = fa.flatMap(f)
      def foldLeft[A, B](fa: MyTree[A], z: B)(f: (B, A) => B): B =
        fa match {
          case Leaf(a)    => f(z, a)
          case Node(l, r) => foldLeft(r, foldLeft(l, z)(f))(f)
        }
    }

  // not stack-safe!
  implicit def myTreeEqual[A](implicit A: Equal[A]): Equal[MyTree[A]] = new Equal[MyTree[A]] {
    def equal(x: MyTree[A], y: MyTree[A]): Boolean =
      (x.size == y.size) && {
        def go(lt: MyTree[A], rt: MyTree[A]): Boolean =
          (lt, rt) match {
            case (Leaf(l), Leaf(r)) if A.equal(l, r) => true
            case (Node(ll, lr), Node(rl, rr))        => go(ll, rl) && go(lr, rr)
            case _                                   => false
          }
        go(x, y)
      }
  }

  // just for learning
  object PureInstances {

    val myTreeFunctor: Functor[MyTree] = new Functor[MyTree] {
      def map[A, B](fa: MyTree[A])(f: A => B): MyTree[B] = fa.map(f)
    }

    val myTreeApplicative: Applicative[MyTree] = new Applicative[MyTree] {
      def pure[A](a: A): MyTree[A] = leaf(a)
      def ap[A, B](fa: MyTree[A])(ff: MyTree[A => B]): MyTree[B] =
        ff match {
          case Leaf(f)           => fa map f
          case Node(left, right) => node(ap(fa)(left), ap(fa)(right))
        }
    }

    val myTreeMonad: Monad[MyTree] = new Monad[MyTree] {
      def pure[A](a: A): MyTree[A]                                   = leaf(a)
      def flatMap[A, B](fa: MyTree[A])(f: A => MyTree[B]): MyTree[B] = fa flatMap f
    }

    val myTreeFoldable: Foldable[MyTree] = new Foldable[MyTree] {
      def foldLeft[A, B](fa: MyTree[A], z: B)(f: (B, A) => B): B =
        fa match {
          case Leaf(a)           => f(z, a)
          case Node(left, right) => foldLeft(right, foldLeft(left, z)(f))(f)
        }
    }

    val myTreeTraverse: Traverse[MyTree] = new Traverse[MyTree] {
      def map[A, B](fa: MyTree[A])(f: A => B): MyTree[B] = fa.map(f)
      def foldLeft[A, B](fa: MyTree[A], z: B)(f: (B, A) => B): B =
        fa match {
          case Leaf(a)           => f(z, a)
          case Node(left, right) => foldLeft(right, foldLeft(left, z)(f))(f)
        }
      def traverse[G[_]: Applicative, A, B](fa: MyTree[A])(f: A => G[B]): G[MyTree[B]] =
        fa match {
          case Leaf(a)           => Applicative[G].map(f(a))(leaf)
          case Node(left, right) => Applicative[G].map2(traverse(left)(f), traverse(right)(f))(node)
        }
    }
  }
}
