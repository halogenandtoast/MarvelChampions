module Marvel.Minion.Minions.UltimateBioServant (
  ultimateBioServant,
  UltimateBioServant (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

ultimateBioServant :: MinionCard UltimateBioServant
ultimateBioServant = minion UltimateBioServant Cards.ultimateBioServant (Sch 1) (Atk 1) (HP 4)

newtype UltimateBioServant = UltimateBioServant (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | [star] +1 ATK for each attachment on it.
instance HasModifiersFor UltimateBioServant where
  getModifiersFor _ (MinionRef mid) (UltimateBioServant a)
    | mid == minionId a = pure [AttackModifier (fromIntegral $ length $ minionAttachments a) | notNull (minionAttachments a)]
  getModifiersFor _ _ _ = pure []

-- | [star] Boost: give the villain a tough status card.
instance RunMessage UltimateBioServant where
  runMessage msg e@(UltimateBioServant attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      villain <- selectJust ActiveVillain
      e <$ enemyBecomesTough villain
    _ -> UltimateBioServant <$> runMessage msg attrs
