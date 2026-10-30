module Marvel.Minion.Minions.BerserkMutate (
  berserkMutate,
  BerserkMutate (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Game.Source

berserkMutate :: MinionCard BerserkMutate
berserkMutate = minion BerserkMutate Cards.berserkMutate (Sch 0) (Atk 2) (HP 3)

newtype BerserkMutate = BerserkMutate (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Quickstrike is on the card. [star] Boost: 1 test counter on the main scheme, then
-- Zola gets +1 SCH and +1 ATK for each test counter there, for this activation (as boost
-- icons, which count for either).
instance RunMessage BerserkMutate where
  runMessage msg e@(BerserkMutate attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      selectOne EncounterMainScheme >>= \case
        Just (SchemeMainSchemeId ms) -> do
          counters <- getMainSchemeCounters ms
          placeCounters ms 1
          case enemy of
            EnemyVillainId vid -> addBoost vid (counters + 1)
            _ -> pure ()
        _ -> pure ()
      pure e
    _ -> BerserkMutate <$> runMessage msg attrs
