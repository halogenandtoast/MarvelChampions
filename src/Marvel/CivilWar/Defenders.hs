module Marvel.CivilWar.Defenders where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.CivilWar.DangerousRecruits (JusticeLikeLightning (..))
import Marvel.CivilWar.MartialLaw (ShieldPatrol (..))
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Minion.Runner
import Marvel.SideScheme.Types
import Marvel.Treachery.Types

newtype Defender = Defender (Attrs Minion)
  deriving anyclass (IsMinion,HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

defenders :: [MinionCard Defender]
defenders = [minion Defender C.cw56184 (Sch 1) (Atk 2) (HP 4),minion Defender C.cw56185 (Sch 2) (Atk 1) (HP 3)]

instance RunMessage Defender where
  runMessage msg e@(Defender a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> e <$ applyStatus you
    Boost (RevealedAsBoost target enemy) | isTarget a target -> e <$ (getBoostedPlayer enemy >>= applyStatus)
    CardSpecific (isTarget a -> True) "afterFriendlyLeaderStatus" (fromJSON -> Success (you,vid,status,before)) -> do
      after <- getCharacterStatusCount (VillainCharacter vid) status
      when (after <= (before :: Natural)) $ giveStatus (IdentityCharacter you) status
      pure e
    Boost inner@(CardSpecific target "afterFriendlyLeaderStatus" _) | isTarget a target -> runMessage inner e
    _ -> Defender <$> runMessage msg a
   where
    applyStatus you = do
      let status = if toCardCode a == "56184" then StunnedStatus else ConfusedStatus
      friendlyLeaderFor you >>= \case
        Nothing -> giveStatus (IdentityCharacter you) status
        Just vid -> do
          before <- getCharacterStatusCount (VillainCharacter vid) status
          giveStatus vid status
          cardSpecific a "afterFriendlyLeaderStatus" (you,vid,status,before)

newtype ProtectTheInnocent = ProtectTheInnocent (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

protectTheInnocent :: TreacheryCard ProtectTheInnocent
protectTheInnocent = treachery ProtectTheInnocent C.cw56186

instance RunMessage ProtectTheInnocent where
  runMessage msg e@(ProtectTheInnocent a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      leader <- enemyLeaderFor you
      already <- maybe (pure False) (fmap (> 0) . (`getCharacterStatusCount` ToughStatus) . VillainCharacter) leader
      traverse_ (`giveStatus` ToughStatus) leader
      minions <- asYou you $ selectList AnyMinion
      traverse_ (`giveStatus` ToughStatus) minions
      when already $ gainSurge a
      pure e
    _ -> ProtectTheInnocent <$> runMessage msg a

streetDefenders :: [TreacheryCard JusticeLikeLightning]
streetDefenders = map (treachery JusticeLikeLightning) [C.cw56187,C.cw56192]

theDefenders :: SideSchemeCard ShieldPatrol
theDefenders = sideScheme ShieldPatrol C.cw56188 (Static 3)
