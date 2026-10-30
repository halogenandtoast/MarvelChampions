module Marvel.Support.Supports.FieldAgent where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Trait (Trait (Shield))
import Marvel.Window qualified as W

fieldAgent :: SupportCard FieldAgent
fieldAgent = supportWith FieldAgent Cards.fieldAgent $ (usesL .~ 3) . (discardIfNoUsesL .~ True)

newtype FieldAgent = FieldAgent (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities FieldAgent where
  getAbilities a =
    [ limitedWindowAbility a 1 (W.AllyWouldTakeDamageOf W.When (AllyWithTrait Shield) FromConsequential)
        HeroInterrupt OwnsThis (ExhaustCost <> UseCost) (runAbility a 1) ]

instance RunMessage FieldAgent where
  runMessage msg s@(FieldAgent attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [ally | W.AllyTakeDamage ally damage <- windows, damageSource damage == FromConsequential] (`preventAllyDamage` 1)
      pure s
    _ -> FieldAgent <$> runMessage msg attrs
