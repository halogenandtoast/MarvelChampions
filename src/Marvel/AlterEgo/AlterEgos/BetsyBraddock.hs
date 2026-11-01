module Marvel.AlterEgo.AlterEgos.BetsyBraddock where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Discard
import Marvel.Projection
import Marvel.Identity.Types (Field (PlayerIdentityDiscard))
import Marvel.Criteria
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Query ()
import Marvel.Question
import Marvel.Payment ()
import Marvel.Resource ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W ()
import Marvel.Keyword qualified as Keyword ()
import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Obligation.Cards qualified as Obligations

betsyBraddock :: AlterEgoCard BetsyBraddock
betsyBraddock = alterEgo BetsyBraddock Cards.betsyBraddock (HP $ Static 10) (HandSize 6) (Rec 3) [Obligations.bodySwapped]

newtype BetsyBraddock = BetsyBraddock (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities BetsyBraddock where
  getAbilities (BetsyBraddock a) =
    [ ability a 1 Setup IsSelf NoCost (runAbility a 1)
    , ability a 2 Action (IsSelf <> ExtendedCardExists (InDiscardOf (IdentityWithId $ alterEgoIdentityId a) (BasicCardMatches $ CardWithTrait Trait.Psionic)))
        (ExhaustUpgradeCost $ UpgradeWithTrait Trait.PsiEnergy <> UpgradeControlledBy (IdentityWithId $ alterEgoIdentityId a)) (runAbility a 2)
    ]

instance RunMessage BetsyBraddock where
  runMessage msg e@(BetsyBraddock a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> e <$ putPsiEnergyIntoPlay you
    RanAbility you (isTarget a -> True) 2 _ _ -> do
      cards <- filter (cardMatch $ CardWithTrait Trait.Psionic) <$> projectMap PlayerIdentityDiscard unDiscard you
      chooseOne you [targeting card (shuffleIntoDeck you [card]) | card <- cards]
      pure e
    _ -> BetsyBraddock <$> runMessage msg a
