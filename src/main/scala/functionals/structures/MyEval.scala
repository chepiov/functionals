package functionals.structures

sealed trait MyEval[+A] extends Serializable {
  def value: A
  def map[B](f: A => B): MyEval[B]             = ???
  def flatMap[B](f: A => MyEval[B]): MyEval[B] = ???
  def memoize: MyEval[A]
}

final case class Now[A](value: A) extends MyEval[A] {
  def memoize: MyEval[A] = this
}

final class Later[A](f: () => A) extends MyEval[A] {
  private[this] var thunk: () => A = f

  lazy val value: A = {
    val result = thunk()
    thunk = null // GC
    result
  }

  def memoize: MyEval[A] = this
}

object Later {
  def apply[A](a: => A): Later[A] = new Later(() => a)
}

final class Always[A](f: () => A) extends MyEval[A] {
  def value: A           = f()
  def memoize: MyEval[A] = new Later(f)
}

object Always {
  def apply[A](a: => A): Always[A] = new Always(() => a)
}

object MyEval {
  def now[A](a: A): MyEval[A]       = Now(a)
  def later[A](a: => A): MyEval[A]  = Later(a)
  def always[A](a: => A): MyEval[A] = Always(a)
}
