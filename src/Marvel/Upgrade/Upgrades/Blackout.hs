module Marvel.Upgrade.Upgrades.Blackout where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Payment
import Marvel.Query
import Marvel.Ref
import Marvel.Resource
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

blackout :: UpgradeCard Blackout
blackout = upgradeWith Blackout Cards.blackout (\attrs -> attrs {upgradeResourceSlots = [Energy, Energy, Mental, Mental, Physical, Physical]})

newtype Blackout = Blackout (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Blackout where
  getAbilities a@(Blackout attrs) =
    [ label (countLabel "metagameSlot" $ fromIntegral $ slot + 1) $
        ability a (fromIntegral $ slot + 1) HeroAction (OwnsThis <> MovableThreatExists)
          (ResourceCost $ Just resource) (runAbility a $ fromIntegral $ slot + 1)
    | (slot, resource) <- zip [0 ..] (upgradeResourceSlots attrs)
    , slot `notElem` upgradeOccupiedSlots attrs
    ]

instance RunMessage Blackout where
  runMessage msg e@(Blackout attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) idx _ payment -> do
      let slot = fromIntegral idx - 1
      for_ (atMay (upgradeResourceSlots attrs) slot) $ \resource -> whenM (paidWith resource payment) do
          schemes <- filterM (canMoveSchemeThreatFrom attrs) =<< selectList (SchemeWithThreat $ GreaterThan $ Static 0)
          chooseOne you [targeting (schemeRef scheme) do
            moveThreatToUpgradeSlot (upgradeId attrs) scheme slot
            cardSpecific attrs "completed" ()
            | scheme <- schemes]
      pure e
    CardSpecific (isTarget attrs -> True) "completed" _ -> do
      when (length (upgradeOccupiedSlots attrs) == length (upgradeResourceSlots attrs)) do
        removeFromPlay attrs
        villains <- selectList VillainEnemy
        chooseOne (upgradeController attrs) [targeting (enemyRef villain) (confuse villain attrs) | villain <- villains]
      pure e
    _ -> Blackout <$> runMessage msg attrs
