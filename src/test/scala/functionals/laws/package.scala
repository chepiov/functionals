package functionals

import functionals.structures._
import functionals.transformers.MyOptionT
import org.scalacheck.{Arbitrary, Gen}

package object laws {

  implicit def arbMyOption[T: Arbitrary]: Arbitrary[MyOption[T]] =
    Arbitrary {
      Arbitrary.arbitrary[Option[T]].map {
        case Some(v) => MySome(v)
        case None    => MyNone
      }
    }

  implicit def arbMyList[T](implicit arb: Arbitrary[List[T]]): Arbitrary[MyList[T]] =
    Arbitrary {
      for {
        xs <- arb.arbitrary
      } yield MyList(xs: _*)
    }

  implicit def arbMyEither[E, A](implicit arbE: Arbitrary[E], arbA: Arbitrary[A]): Arbitrary[MyEither[E, A]] =
    Arbitrary {
      Arbitrary.arbitrary[Either[E, A]].map {
        case Left(e)  => MyLeft(e)
        case Right(a) => MyRight(a)
      }
    }

  implicit def arbMyValidated[E, A](implicit arbE: Arbitrary[E], arbA: Arbitrary[A]): Arbitrary[MyValidated[E, A]] =
    Arbitrary {
      Arbitrary.arbitrary[Either[E, A]].map {
        case Left(e)  => MyInvalid(e)
        case Right(a) => MyValid(a)
      }
    }

  implicit def arbMyTree[T](implicit arb: Arbitrary[T]): Arbitrary[MyTree[T]] =
    Arbitrary {
      val genLeaf: Gen[MyTree[T]] =
        for {
          v <- arb.arbitrary
        } yield MyTree.leaf(v)

      def genNode(sz: Int): Gen[MyTree[T]] =
        for {
          left  <- genTree(sz - 1)
          right <- genTree(sz - 2)
        } yield MyTree.node(left, right)

      def genTree(sz: Int): Gen[MyTree[T]] =
        if (sz <= 0) genLeaf else Gen.oneOf(genLeaf, genNode(sz))

      Gen.sized(sz => genTree(sz))
    }

  implicit def arbMyOptionT[T](implicit arb: Arbitrary[MyList[MyOption[T]]]): Arbitrary[MyOptionT[MyList, T]] =
    Arbitrary {
      for {
        xs <- arb.arbitrary
      } yield MyOptionT(xs)
    }
}
