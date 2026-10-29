module Marvel.Support.Supports.MunitionsBunker (munitionsBunker, MunitionsBunker (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

munitionsBunker :: SupportCard MunitionsBunker
munitionsBunker = support MunitionsBunker Cards.munitionsBunker

newtype MunitionsBunker = MunitionsBunker (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MunitionsBunker where
  getAbilities a@(MunitionsBunker attrs) =
    ability a 1 AlterEgoAction OwnsThis ExhaustCost (runAbility a 1)
      : [ability a 2 HeroAction OwnsThis ExhaustCost (runAbility a 2) | supportUses attrs > 0]

instance RunMessage MunitionsBunker where
  runMessage msg s@(MunitionsBunker attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> pure $ MunitionsBunker (attrs & usesL +~ 2)
    RanAbility ident (isTarget attrs -> True) 2 _ _ -> do
      placeCounters ident (supportUses attrs)
      pure $ MunitionsBunker (attrs & usesL .~ 0)
    _ -> MunitionsBunker <$> runMessage msg attrs
