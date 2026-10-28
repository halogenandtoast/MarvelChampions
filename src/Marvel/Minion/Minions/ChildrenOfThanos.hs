-- | The Children of Thanos modular set (The Mad Titan's Shadow): the minions.
module Marvel.Minion.Minions.ChildrenOfThanos (
  corvusGlaiveMinion,
  CorvusGlaiveMinion (..),
  proximaMidnightMinion,
  ProximaMidnightMinion (..),
  ebonyMawMinion,
  EbonyMawMinion (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Game.Source
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

-- | "Discard an X or Y you control", the player being the one the activation is against.
discardForBoost :: (ReverseQueue m) => EnemyId -> (IdentityId -> m [Target]) -> m ()
discardForBoost enemy cards = do
  you <- getBoostedPlayer enemy
  chooseAndDiscardControlled you =<< cards you

corvusGlaiveMinion :: MinionCard CorvusGlaiveMinion
corvusGlaiveMinion = minion CorvusGlaiveMinion Cards.corvusGlaiveMinion (Sch 2) (Atk 2) (HP 4)

-- | Retaliate 1. Toughness (its keywords).
newtype CorvusGlaiveMinion = CorvusGlaiveMinion (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Boost: discard an ally or support you control.
instance RunMessage CorvusGlaiveMinion where
  runMessage msg e@(CorvusGlaiveMinion attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      discardForBoost enemy \you -> do
        allies <- selectList (AllyControlledBy $ IdentityWithId you)
        supports <- selectList (SupportControlledBy $ IdentityWithId you)
        pure $ map AllyRef allies <> map SupportRef supports
      pure e
    _ -> CorvusGlaiveMinion <$> runMessage msg attrs

proximaMidnightMinion :: MinionCard ProximaMidnightMinion
proximaMidnightMinion = minion ProximaMidnightMinion Cards.proximaMidnightMinion (Sch 1) (Atk 3) (HP 5)

newtype ProximaMidnightMinion = ProximaMidnightMinion (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Proxima Midnight's attacks gain piercing.
instance HasModifiersFor ProximaMidnightMinion where
  getModifiersFor _ (MinionRef mid) (ProximaMidnightMinion a) | mid == minionId a = pure [KeywordModifier Piercing]
  getModifiersFor _ _ _ = pure []

-- | [star] Boost: discard an ally or upgrade you control.
instance RunMessage ProximaMidnightMinion where
  runMessage msg e@(ProximaMidnightMinion attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      discardForBoost enemy \you -> do
        allies <- selectList (AllyControlledBy $ IdentityWithId you)
        upgrades <- selectList (UpgradeControlledBy $ IdentityWithId you)
        pure $ map AllyRef allies <> map UpgradeRef upgrades
      pure e
    _ -> ProximaMidnightMinion <$> runMessage msg attrs

ebonyMawMinion :: MinionCard EbonyMawMinion
ebonyMawMinion = minion EbonyMawMinion Cards.ebonyMawMinion (Sch 1) (Atk 1) (HP 6)

-- | Villainous (its keyword).
newtype EbonyMawMinion = EbonyMawMinion (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Boost: give this enemy 1 additional boost card: the enemy activating, Ebony
-- Maw himself or the villain.
instance RunMessage EbonyMawMinion where
  runMessage msg e@(EbonyMawMinion attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      dealBoost enemy
      pure e
    _ -> EbonyMawMinion <$> runMessage msg attrs
