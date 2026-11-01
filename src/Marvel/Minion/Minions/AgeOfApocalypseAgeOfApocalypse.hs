module Marvel.Minion.Minions.AgeOfApocalypseAgeOfApocalypse where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Ability (HasAbilities)
import Marvel.AgeOfApocalypse.MissionTypes
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Hp
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner ()
import Marvel.Minion.Types
import Marvel.Modifier (HasModifiersFor)
import Marvel.Stats

newtype AgeOfApocalypseMinion = AgeOfApocalypseMinion (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

agentOfApocalypse :: MinionCard AgeOfApocalypseMinion
agentOfApocalypse = minion AgeOfApocalypseMinion Cards.agentOfApocalypse (Sch 2) (Atk 2) (HP 3)

instance RunMessage AgeOfApocalypseMinion where
  runMessage msg e@(AgeOfApocalypseMinion a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      mission <- getMission
      let available = maybe False (not . (.missionFinished)) mission
      chooseOne you $
        [labeled "$label.aoaAddAgentToMission" (cardSpecific GameRef "aoaMissionMinionEntered" mid) | available]
        <> [labeled "$label.aoaAgentActivates" (minionActivatesAgainst mid you)]
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      mission <- getMission
      for_ mission $ \m -> unless (null m.missionAllies) $ chooseOrRunOne you [targeting (AllyRef aid) (damageCharacter aid (SideSchemeRef m.missionScheme) $ toDamage 1 FromAbility) | aid <- m.missionAllies]
      dealBoost (enemyRef enemy)
      pure e
    _ -> AgeOfApocalypseMinion <$> runMessage msg a
