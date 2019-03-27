package functionals.syntax

object semigroup    extends SemigroupSyntax
object monoid       extends MonoidSyntax
object functor      extends FunctorSyntax
object apply        extends ApplySyntax
object applicative  extends ApplicativeSyntax
object monad        extends MonadSyntax
object foldable     extends FoldableSyntax
object traverse     extends TraverseSyntax
object semigroupK   extends SemigroupKSyntax
object monoidK      extends MonoidKSyntax
object monadFilter  extends MonadFilterSyntax
object monadCombine extends MonadCombineSyntax
object equal        extends EqualSyntax

object all
    extends SemigroupSyntax with MonoidSyntax with FunctorSyntax with ApplySyntax with ApplicativeSyntax
    with MonadSyntax with FoldableSyntax with TraverseSyntax with SemigroupKSyntax with MonoidKSyntax
    with MonadFilterSyntax with MonadCombineSyntax with EqualSyntax

object withParents {}
