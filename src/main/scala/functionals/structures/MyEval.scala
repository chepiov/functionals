package functionals.structures

sealed trait MyEval[+A] extends Serializable {
  def value: A
  def map[B](f: A => B): MyEval[B]             = flatMap(a => Now(f(a)))
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
  def now[A](a: A): MyEval[A]              = Now(a)
  def later[A](a: => A): MyEval[A]         = Later(a)
  def always[A](a: => A): MyEval[A]        = Always(a)
  def defer[A](a: => MyEval[A]): MyEval[A] = new MyEval.Defer[A](() => a) {}

  sealed abstract class Defer[A](val thunk: () => MyEval[A]) extends MyEval[A] {
    def memoize: MyEval[A] = Memoize(this)
    def value: A           = evaluate(this)
  }

  sealed abstract class FlatMap[A] extends MyEval[A] { self =>
    type Start
    val start: () => MyEval[Start]
    val run: Start => MyEval[A]

    def memoize: MyEval[A] = Memoize(this)
    def value: A           = evaluate(this)
  }

  private case class Memoize[A](eval: MyEval[A]) extends MyEval[A] {
    var result: Option[A]  = None
    def memoize: MyEval[A] = this
    def value: A = result match {
      case Some(a) => a
      case None =>
        val a = evaluate(this)
        result = Some(a)
        a
    }
  }

  private def evaluate[A](e: MyEval[A]): A = {
    ???
  }
}
