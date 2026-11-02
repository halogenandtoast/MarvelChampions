module Marvel.CivilWar.MightyAvengers where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Game.Source
import Marvel.Hp
import Marvel.Message.Lifted
import Marvel.Minion.Runner

newtype MightyAvenger = MightyAvenger (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

definitions :: [CardDef]
definitions = [C.cw56073,C.cw56074,C.cw56075]

minions :: [MinionCard MightyAvenger]
minions = map make definitions
 where
  make def = let (sch,atk,hp) = C.printedMinionStats (toCardCode def)
             in minion MightyAvenger def (Sch sch) (Atk atk) (HP hp)

instance RunMessage MightyAvenger where
  runMessage msg e@(MightyAvenger a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> e <$ revealed you
    Boost (RevealedAsBoost target enemy) | isTarget a target -> e <$ (getBoostedPlayer enemy >>= revealed)
    _ -> MightyAvenger <$> runMessage msg a
   where
    revealed you = case toCardCode a of
      "56073" -> enemyLeaderFor you >>= traverse_ enemyBecomesTough
      "56074" -> placeThreatOnEach a 1 EncounterMainScheme
      "56075" -> damageIdentity you a (toDamage 1 FromAbility)
      _ -> pure ()
