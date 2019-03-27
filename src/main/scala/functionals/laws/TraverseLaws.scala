package functionals.laws

import functionals.MyId
import functionals.typeclasses.Traverse

trait TraverseLaws[F[_]] extends FunctorLaws[F] with FoldableLaws[F] {
  import IsEqual.ops._
  import functionals.syntax.withParent.traverse._

  implicit override val typeClass: Traverse[F]

  def traverseIdentity[A, B](fa: F[A], f: A => B): IsEqual[MyId[F[B]]] =
    fa.traverse[MyId, B](f) =?= typeClass.map(fa)(f)
}

object TraverseLaws {
  def apply[F[_]: Traverse]: TraverseLaws[F] = new TraverseLaws[F] with FoldableLaws[F] {
    val typeClass: Traverse[F] = Traverse[F]
  }
}
