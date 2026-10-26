module Marvel.Upgrade.Upgrades.ScarletWitchsCrest (
  scarletWitchsCrest,
  ScarletWitchsCrest (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

scarletWitchsCrest :: UpgradeCard ScarletWitchsCrest
scarletWitchsCrest = upgrade ScarletWitchsCrest Cards.scarletWitchsCrest

newtype ScarletWitchsCrest = ScarletWitchsCrest (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Interrupt: When boost icons on an encounter card are counted, exhaust Scarlet
-- Witch's Crest → increase or decrease the number of boost icons on that card by 1 for
-- this count. In either form.
instance HasAbilities ScarletWitchsCrest where
  getAbilities a = [limitedWindowAbility a 1 BoostIconsCounted Interrupt OwnsThis ExhaustCost (runAbility a 1)]

-- | It changes the count as it stands: after Chaos Control, the count Chaos Control made
-- (the owner's ruling). No fewer than 0.
instance RunMessage ScarletWitchsCrest where
  runMessage msg u@(ScarletWitchsCrest attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 [CountingBoostIcons card] _ -> do
      n <- fromMaybe 0 <$> getBoostIconCount card
      chooseOne ident $
        labeled "$label.increaseBoostIcons" (cardSpecific attrs "change" (card, True))
          : [labeled "$label.decreaseBoostIcons" (cardSpecific attrs "change" (card, False)) | n > 0]
      pure u
    -- a step of its own: a queue edit in a choice would be made as it is asked
    CardSpecific (isTarget attrs -> True) "change" (fromJSON -> Success (card, increase)) -> do
      changeBoostIconCount card (if increase then (+ 1) else subtract 1)
      pure u
    _ -> ScarletWitchsCrest <$> runMessage msg attrs
