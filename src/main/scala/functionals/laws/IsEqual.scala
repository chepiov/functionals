package functionals.laws

import functionals.typeclasses.Equal
import functionals.syntax.all._

final case class IsEqual[A](lhs: A, rhs: A) {
  def isEqual(implicit eq: Equal[A]): Boolean = lhs === rhs
}

case object IsEqual {
  object ops {
    implicit class IsEqualSyntax[A](val lhs: A) extends AnyVal {
      def =?=(rhs: A): IsEqual[A] = IsEqual(lhs, rhs)
    }
  }
}
