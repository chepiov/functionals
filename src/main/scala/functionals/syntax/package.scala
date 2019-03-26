package functionals.syntax

import functionals.typeclasses._

object semigroup    extends Semigroup.ToSemigroupOps
object monoid       extends Monoid.ToMonoidOps
object functor      extends Functor.ToFunctorOps
object apply        extends Apply.ToApplyOps
object applicative  extends Applicative.ToApplicativeOps
object monad        extends Monad.ToMonadOps
object foldable     extends Foldable.ToFoldableOps
object traverse     extends Traverse.ToTraverseOps
object semigroupK   extends SemigroupK.ToSemigroupKOps
object monoidK      extends MonoidK.ToMonoidKOps
object monadFilter  extends MonadFilter.ToMonadFilterOps
object monadCombine extends MonadCombine.ToMonadCombineOps
object equal        extends Equal.ToEqualOps

object all
    extends Semigroup.ToSemigroupOps with Monoid.ToMonoidOps with Functor.ToFunctorOps with Apply.ToApplyOps
    with Applicative.ToApplicativeOps with Monad.ToMonadOps with Foldable.ToFoldableOps with Traverse.ToTraverseOps
    with SemigroupK.ToSemigroupKOps with MonoidK.ToMonoidKOps with MonadFilter.ToMonadFilterOps
    with MonadCombine.ToMonadCombineOps with Equal.ToEqualOps
