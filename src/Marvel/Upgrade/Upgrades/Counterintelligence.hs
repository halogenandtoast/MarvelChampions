module Marvel.Upgrade.Upgrades.Counterintelligence (
  counterintelligence,
  Counterintelligence (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers qualified as Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

counterintelligence :: UpgradeCard Counterintelligence
counterintelligence = upgrade Counterintelligence Cards.counterintelligence

newtype Counterintelligence = Counterintelligence (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Counterintelligence where
  getAbilities a =
    [ limitedWindowAbility
        a
        1
        (ThreatWouldBePlaced AnyThreatSource Matchers.MainScheme)
        Interrupt
        OwnsThis
        (DiscardCost $ toTarget a)
        (runAbility a 1)
    ]

-- | Prevent 3 of the threat about to be placed.
instance RunMessage Counterintelligence where
  runMessage msg u@(Counterintelligence attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [ThreatPlaced _ (SchemeMainSchemeId mid) _] _ -> do
      let prevent = \case
            MainSchemeMessage mid' (MainSchemePlaceThreat n) ->
              [MainSchemeMessage mid' (MainSchemePlaceThreat $ n - 3) | n > 3]
            other -> [other]
      replaceMatchingMessage prevent \case
        MainSchemeMessage mid' (MainSchemePlaceThreat _) -> mid' == mid
        _ -> False
      pure u
    _ -> Counterintelligence <$> runMessage msg attrs
