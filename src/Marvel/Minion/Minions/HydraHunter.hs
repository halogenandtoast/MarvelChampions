module Marvel.Minion.Minions.HydraHunter (
  hydraHunter,
  HydraHunter (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

hydraHunter :: MinionCard HydraHunter
hydraHunter = minion HydraHunter Cards.hydraHunter (Sch 2) (Atk 2) (HP 3)

newtype HydraHunter = HydraHunter (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | "Hydra Hunter's attacks gain piercing and ranged."
instance HasModifiersFor HydraHunter where
  getModifiersFor _ (MinionRef mid) (HydraHunter a)
    | mid == minionId a = pure [KeywordModifier Piercing, KeywordModifier Ranged]
  getModifiersFor _ _ _ = pure []

instance RunMessage HydraHunter where
  runMessage msg e@(HydraHunter attrs) = runQueueT $ case msg of
    -- Boost: "If you are in hero form, take 1 damage. Otherwise place 1 threat on the
    -- main scheme."
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      you <- getActivePlayerId
      hero <- identityMatches HeroIdentity you
      if hero
        then damageIdentity you attrs (toDamage 1 FromAbility)
        else placeThreatOnEach attrs 1 EncounterMainScheme
      pure e
    _ -> HydraHunter <$> runMessage msg attrs
