module Marvel.Minion.Minions.Bantam where

import Marvel.Prelude
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Query
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Window ()
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

bantam :: MinionCard Bantam
bantam = minion Bantam Cards.bantam (Sch 2) (Atk 2) (HP 3)

newtype Bantam = Bantam (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)



instance RunMessage Bantam where
  runMessage msg a@(Bantam attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      portal <- selectOne $ SideSchemeIs SideSchemes.portalThroughTime
      case portal of
        Just sid -> placeThreat sid 2
        Nothing -> searchEverywhereAndRevealEncounterCard you $ CardIs SideSchemes.portalThroughTime
      pure a
    _ -> Bantam <$> runMessage msg attrs
