package functionals.typeclasses

trait Equal[A] {
  def equal(x: A, y: A): Boolean
  final def notEqual(x: A, y: A): Boolean = !equal(x, y)
}

case object Equal {

  def apply[A](implicit A: Equal[A]): Equal[A] = A

  def natural[A]: Equal[A] = new Equal[A] {
    def equal(x: A, y: A): Boolean = x == y
  }

  def instance[A](f: (A, A) => Boolean): Equal[A] = new Equal[A] {
    def equal(x: A, y: A): Boolean = f(x, y)
  }

  trait Ops[A] {
    def typeClassInstance: Equal[A]
    def self: A

    def ===(y: A): Boolean = typeClassInstance.equal(self, y)
    def =!=(y: A): Boolean = typeClassInstance.notEqual(self, y)
  }

  trait ToEqualOps {
    implicit def toEqualOps[A](target: A)(implicit tc: Equal[A]): Ops[A] =
      new Ops[A] {
        def typeClassInstance: Equal[A] = tc
        def self: A                     = target
      }
  }

  object ops extends ToEqualOps
}
