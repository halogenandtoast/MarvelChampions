module Marvel.Support.Supports.AngelsAerie where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Payment
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

angelsAerie :: SupportCard AngelsAerie
angelsAerie = support AngelsAerie Cards.angelsAerie
newtype AngelsAerie = AngelsAerie (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AngelsAerie where
  getAbilities a@(AngelsAerie attrs) =
    [limitedWindowAbility a 1 (W.HeroDefended W.After (IdentityWithId $ supportController attrs) AnyEnemy)
      Response OwnsThis NoCost (runAbility a 1)]
    <> [ability a 2 AlterEgoAction (OwnsThis <> SelfMatches (IdentityWithAnyDamage <> IdentityCanBeHealed)) (ExactCountersCost $ supportUses attrs) (runAbility a 2) | supportUses attrs > 0]

instance RunMessage AngelsAerie where
  runMessage msg a@(AngelsAerie attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ addSupportUses (supportId attrs) 1
    RanAbility you (isTarget attrs -> True) 2 _ payment -> do
      heal you (sum [n | CountersPayment n <- payments payment])
      pure a
    _ -> AngelsAerie <$> runMessage msg attrs
