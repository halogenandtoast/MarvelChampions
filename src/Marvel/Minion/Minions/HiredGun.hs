module Marvel.Minion.Minions.HiredGun (
  hiredGun,
  HiredGun (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Environment.Environments.CriminalEnterprise (infamyOrMadness, placeInfamy)
import Marvel.Environment.Cards qualified as Environments
import Marvel.Matchers

hiredGun :: MinionCard HiredGun
hiredGun = minion HiredGun Cards.hiredGun (Sch 2) (Atk 2) (HP 3)

newtype HiredGun = HiredGun (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage HiredGun where
  runMessage msg e@(HiredGun attrs) = runQueueT $ case msg of
    -- When Revealed: give the villain a facedown boost card, or place 2 infamy counters
    -- on Criminal Enterprise, which cannot be chosen while it is State of Madness
    MinionMessage ident (RevealMinion you) | ident == minionId attrs -> do
      villain <- selectJust ActiveVillain
      infamy <- selectAny (EnvironmentIs Environments.criminalEnterprise)
      chooseOrRunOne you $
        labeled "$label.giveTheVillainAFacedownBoostCard" (dealBoost villain)
          : [labeled "$label.place2InfamyCounters" (void $ placeInfamy 2) | infamy]
      pure e
    Boost (RevealedAsBoost target _) | isTarget attrs target -> e <$ infamyOrMadness 1
    _ -> HiredGun <$> runMessage msg attrs
