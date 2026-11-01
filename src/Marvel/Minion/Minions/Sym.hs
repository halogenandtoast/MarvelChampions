module Marvel.Minion.Minions.Sym where

import Marvel.Prelude
import Marvel.Query
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Matchers
import Marvel.SideScheme.Cards qualified as Schemes

sym :: MinionCard Sym
sym = minion Sym Cards.sym (Sch 2) (Atk 2) (HP 5)

newtype Sym = Sym (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Sym where
  runMessage msg a@(Sym attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion _) | mid == minionId attrs -> do
      ruler <- selectOne (SideSchemeIs Schemes.rulerOfLimbo)
      case ruler of
        Just scheme -> placeThreatOn attrs scheme 2
        Nothing -> placeThreatOnEach attrs 2 MainScheme
      pure a
    _ -> Sym <$> runMessage msg attrs
