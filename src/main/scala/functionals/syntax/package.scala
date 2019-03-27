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

object withParent {
  object semigroup    extends SemigroupAllSyntax
  object monoid       extends MonoidAllSyntax
  object functor      extends FunctorAllSyntax
  object apply        extends ApplyAllSyntax
  object applicative  extends ApplicativeAllSyntax
  object monad        extends MonadAllSyntax
  object foldable     extends FoldableAllSyntax
  object traverse     extends TraverseAllSyntax
  object semigroupK   extends SemigroupKAllSyntax
  object monoidK      extends MonoidKAllSyntax
  object monadFilter  extends MonadFilterAllSyntax
  object monadCombine extends MonadCombineAllSyntax
  object equal        extends EqualAllSyntax
}
