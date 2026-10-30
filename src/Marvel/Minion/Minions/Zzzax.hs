module Marvel.Minion.Minions.Zzzax where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Card.PlayerCard (printedResourcesFor)
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Identity.Types (PlayerIdentity, Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Projection
import Marvel.Resource

zzzax :: MinionCard Zzzax
zzzax = minion Zzzax Cards.zzzax (Sch 2) (Atk 2) (HP 4)

newtype Zzzax = Zzzax (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- In-play printed icons count individually, not cards or resource abilities.
controlledEnergy :: HasGame m => IdentityId -> m Natural
controlledEnergy you = do
  allies <- selectList (AllyControlledBy $ IdentityWithId you)
  supports <- selectList (SupportControlledBy $ IdentityWithId you)
  upgrades <- selectList (UpgradeControlledBy $ IdentityWithId you)
  let targets = map AllyRef allies <> map SupportRef supports <> map UpgradeRef upgrades
  defs <- getsGame $ \g -> mapMaybe (abilityCardDef g) targets
  pure $ fromIntegral $ count (== Energy) $ concatMap printedResources defs

handEnergy :: (HasGame m, Projection m PlayerIdentity) => IdentityId -> m Natural
handEnergy you = do
  hand <- unHand <$> project PlayerIdentityHand you
  fromIntegral . count (== Energy) . concat <$> traverse printedResourcesFor hand

instance HasModifiersFor Zzzax where
  getModifiersFor _ target (Zzzax attrs) | isTarget attrs target = do
    n <- controlledEnergy (minionEngagedIdentity attrs)
    pure [AttackModifier n, HitPointModifier n]
  getModifiersFor _ _ _ = pure []

instance RunMessage Zzzax where
  runMessage msg z@(Zzzax attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      you <- getActivePlayerId
      n <- handEnergy you
      when (n >= 2) $ putBoostIntoPlay target you
      pure z
    _ -> Zzzax <$> runMessage msg attrs
