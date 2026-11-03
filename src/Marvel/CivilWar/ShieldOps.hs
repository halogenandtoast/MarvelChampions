module Marvel.CivilWar.ShieldOps where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Runner
import Marvel.SideScheme.Types
import Marvel.Treachery.Types
import Marvel.Window qualified as W

newtype ShieldOperator = ShieldOperator (Attrs Minion)
  deriving anyclass (IsMinion,HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

shieldOperator :: MinionCard ShieldOperator
shieldOperator = let (sch,atk,hp) = C.printedMinionStats "57013"
  in minion ShieldOperator C.cw57013 (Sch sch) (Atk atk) (HP hp)

instance RunMessage ShieldOperator where
  runMessage msg e@(ShieldOperator a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      schemes <- asYou you $ selectList EncounterMainScheme
      for_ schemes $ \scheme -> placeThreatOn a scheme 2
      pure e
    _ -> ShieldOperator <$> runMessage msg a

newtype StrategicOverwatch = StrategicOverwatch (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

strategicOverwatch :: TreacheryCard StrategicOverwatch
strategicOverwatch = treachery StrategicOverwatch C.cw57014

instance RunMessage StrategicOverwatch where
  runMessage msg e@(StrategicOverwatch a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      enemyLeaderFor you >>= traverse_ enemyBecomesTough
      minions <- asYou you $ selectList AnyMinion
      traverse_ (`giveStatus` ToughStatus) minions
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> e <$ giveStatus enemy ToughStatus
    _ -> StrategicOverwatch <$> runMessage msg a

newtype HomelandSecurity = HomelandSecurity (Attrs SideScheme)
  deriving anyclass HasModifiersFor
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

homelandSecurity :: SideSchemeCard HomelandSecurity
homelandSecurity = sideScheme HomelandSecurity C.cw57016 (Static 3)

instance IsSideScheme HomelandSecurity where
  sideSchemeAbilities a = [limitedAbility a 1 (PerRound 1) ForcedInterrupt NoCriteria NoCost (runAbility a 1)
    & windowL ?~ W.EncounterCardRevealedBy W.When AnyIdentity (EncounterCardMatching $ CardWithType MinionType)]

instance RunMessage HomelandSecurity where
  runMessage msg e@(HomelandSecurity a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [(you,card) | W.EncounterCardRevealed _ you card <- windows] $ \(you,card) -> giveRevealedCardSurge you card
      pure e
    _ -> HomelandSecurity <$> runMessage msg a
