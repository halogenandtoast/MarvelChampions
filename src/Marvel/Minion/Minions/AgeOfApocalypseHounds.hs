module Marvel.Minion.Minions.AgeOfApocalypseHounds where

import Marvel.Prelude
import Marvel.Card.Side
import Marvel.Matchers
import Marvel.Query
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Card
import Marvel.Entity
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Ability (HasAbilities)
import Marvel.Modifier ()
import Marvel.Ref ()
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted ()

newtype AoaHoundsMinion = AoaHoundsMinion (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ahab :: MinionCard AoaHoundsMinion
ahab = minion AoaHoundsMinion Cards.ahab (Sch 2) (Atk 3) (HP 5)

hound :: MinionCard AoaHoundsMinion
hound = minion AoaHoundsMinion Cards.hound (Sch 1) (Atk 2) (HP 2)


instance RunMessage AoaHoundsMinion where
  runMessage msg a@(AoaHoundsMinion attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      if toCardCode attrs == "45097" then do
        scheme <- selectOne $ SideSchemeIs SideSchemes.releaseTheHounds
        case scheme of
          Just sid -> placeThreat sid 3
          Nothing -> searchEverywhereAndRevealEncounterCard you $ CardIs SideSchemes.releaseTheHounds
      else do
        hero <- identityMatches HeroIdentity you
        if hero then enemyAttacks (minionId attrs) you else changeToForm you A
      pure a
    _ -> AoaHoundsMinion <$> runMessage msg attrs
