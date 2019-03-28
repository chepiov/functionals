package functionals.laws

import functionals.structures.{MyList, MyReader, MyState}
import functionals.transformers.MyReaderT
import functionals.typeclasses.{Equal, Monad}
import org.scalacheck.Arbitrary
import org.scalatest.Assertion

abstract class MonadWithoutEqualSpec[F[_], S, A, B, C](name: String, monad: Monad[F])(
    implicit arbS: Arbitrary[S],
    arbA: Arbitrary[A],
    arbFA: Arbitrary[F[A]],
    arbAB: Arbitrary[A => B],
    arbBC: Arbitrary[B => C],
    arbFAB: Arbitrary[F[A => B]],
    arbAFB: Arbitrary[A => F[B]],
    arbBFC: Arbitrary[B => F[C]]
) extends LawsSpec {

  implicit def eqAll[T]: Equal[T] = new Equal[T] {
    def equal(x: T, y: T): Boolean = true
  }

  implicit def F: Monad[F] = monad

  lazy val laws: MonadLaws[F] = MonadLaws[F]

  def check[BB](start: S, eq: IsEqual[F[BB]]): Assertion

  property(s"A Functor[$name] should satisfy identity law") {
    forAll { (start: S, fa: F[A]) =>
      check(start, laws.functorIdentity(fa))
    }
  }

  property(s"A Functor[$name] should satisfy composition law") {
    forAll { (start: S, fa: F[A], f: A => B, g: B => C) =>
      check(start, laws.functorComposition(fa, f, g))
    }
  }

  property(s"An Applicative[$name] should satisfy homomorphism law") {
    forAll { (start: S, a: A, f: A => B) =>
      check(start, laws.homomorphism(a, f))
    }
  }

  property(s"An Applicative[$name] should satisfy identity law") {
    forAll { (start: S, fa: F[A]) =>
      check(start, laws.identity(fa))
    }
  }

  property(s"An Applicative[$name] should satisfy interchange law") {
    forAll { (start: S, a: A, ff: F[A => B]) =>
      check(start, laws.interchange(a, ff))
    }
  }

  property(s"An Applicative[$name] should satisfy mapConsistence law") {
    forAll { (start: S, fa: F[A], f: A => B) =>
      check(start, laws.mapConsistence(fa, f))
    }
  }

  property(s"A Monad[$name] should satisfy leftIdentity law") {
    forAll { (start: S, a: A, f: A => F[B]) =>
      check(start, laws.leftIdentity(a, f))
    }
  }

  property(s"A Monad[$name] should satisfy rightIdentity law") {
    forAll { (start: S, fa: F[A]) =>
      check(start, laws.rightIdentity(fa))
    }
  }

  property(s"A Monad[$name] should satisfy flatMapAssociativity law") {
    forAll { (start: S, fa: F[A], f: A => F[B], g: B => F[C]) =>
      check(start, laws.flatMapAssociativity(fa, f, g))
    }
  }
}

class MyStateSpec
    extends MonadWithoutEqualSpec[({ type l[X] = MyState[String, X] })#l, String, Int, String, Long](
      "MyState",
      MyState.myStateMonad[String]) {
  def check[A](start: String, eq: IsEqual[MyState[String, A]]): Assertion = {
    val (result1, s1) = eq.lhs.run(start)
    val (result2, s2) = eq.rhs.run(start)
    s1 should be(s2)
    result1 should be(result2)
  }
}

class MyReaderSpec
    extends MonadWithoutEqualSpec[({ type l[X] = MyReader[String, X] })#l, String, Int, String, Long](
      "MyReader",
      MyReader.myReaderMonad[String]
    ) {
  def check[A](start: String, eq: IsEqual[MyReader[String, A]]): Assertion = {
    val result1 = eq.lhs.run(start)
    val result2 = eq.rhs.run(start)
    result1 should be(result2)
  }
}

class MyReaderTSpec
    extends MonadWithoutEqualSpec[({ type l[X] = MyReaderT[MyList, String, X] })#l, String, Int, String, Long](
      "MyReaderT",
      MyReaderT.myReaderTMonad[MyList, String]
    ) {
  def check[A](start: String, eq: IsEqual[MyReaderT[MyList, String, A]]): Assertion = {
    val result1 = eq.lhs.run(start)
    val result2 = eq.lhs.run(start)
    MyList.myListEqual[A].equal(result1, result2) should be(true)
  }
}
