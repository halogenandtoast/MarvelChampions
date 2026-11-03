module Marvel.CivilWar.Thunderbolts where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.CloakAndDagger (damageControlledCharacter)
import Marvel.CivilWar.DangerousRecruits qualified as Shared
import Marvel.CivilWar.Helpers
import Marvel.Damage
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Runner
import Marvel.SideScheme.Types
import Marvel.Trait qualified as T
import Marvel.Treachery.Types

newtype Thunderbolt = Thunderbolt (Attrs Minion)
  deriving anyclass (IsMinion,HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

thunderbolts :: [MinionCard Thunderbolt]
thunderbolts = map make [C.cw57017,C.cw57018,C.cw57019]
 where
  make def = let (sch,atk,hp) = C.printedMinionStats (toCardCode def)
    in minion Thunderbolt def (Sch sch) (Atk atk) (HP hp)

instance RunMessage Thunderbolt where
  runMessage msg e@(Thunderbolt a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> e <$ applyEffect you 2
    Boost (RevealedAsBoost target enemy) | isTarget a target -> e <$ (getBoostedPlayer enemy >>= (`applyEffect` 1))
    CardSpecific (isTarget a -> True) "afterFriendlyLeaderStatus" (fromJSON -> Success (you,vid,status,before)) -> do
      after <- getCharacterStatusCount (VillainCharacter vid) status
      when (after <= (before :: Natural)) $ giveStatus (IdentityCharacter you) status
      pure e
    Boost inner@(CardSpecific target "afterFriendlyLeaderStatus" _) | isTarget a target -> runMessage inner e
    _ -> Thunderbolt <$> runMessage msg a
   where
    applyEffect you fallbackDamage
      | toCardCode a == "57019" = friendlyLeaderFor you >>= \case
          Nothing -> damageControlledCharacter you a fallbackDamage
          Just vid -> dealDamage vid a (toDamage 2 FromAbility)
      | otherwise = do
          let status = if toCardCode a == "57017" then StunnedStatus else ConfusedStatus
          friendlyLeaderFor you >>= \case
            Nothing -> giveStatus (IdentityCharacter you) status
            Just vid -> do
              before <- getCharacterStatusCount (VillainCharacter vid) status
              giveStatus vid status
              cardSpecific a "afterFriendlyLeaderStatus" (you,vid,status,before)

justiceLikeLightning :: TreacheryCard Shared.JusticeLikeLightning
justiceLikeLightning = treachery Shared.JusticeLikeLightning C.cw57020

newtype TheThunderbolts = TheThunderbolts (Attrs SideScheme)
  deriving anyclass IsSideScheme
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

theThunderbolts :: SideSchemeCard TheThunderbolts
theThunderbolts = sideScheme TheThunderbolts C.cw57021 (Static 3)

instance HasModifiersFor TheThunderbolts where
  getModifiersFor _ (SideSchemeRef sid) (TheThunderbolts a) | sid == sideSchemeId a = do
    protected <- selectAny $ MinionWithTrait T.Thunderbolt
    pure [ThreatCannotBeRemoved | protected]
  getModifiersFor _ _ _ = pure []

instance RunMessage TheThunderbolts where
  runMessage msg (TheThunderbolts a) = runQueueT $ TheThunderbolts <$> runMessage msg a
