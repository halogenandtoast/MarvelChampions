module Marvel.Minion.Minions.DarkPhoenix where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.GameValue ()
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Window qualified as W

darkPhoenix :: MinionCard DarkPhoenix
darkPhoenix = minion DarkPhoenix Cards.darkPhoenix (Sch 2) (Atk 2) (HP 12)

newtype DarkPhoenix = DarkPhoenix (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage DarkPhoenix where
  runMessage msg e@(DarkPhoenix attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      searchEverywhereAndRevealEncounterCard you (CardIs SideSchemes.consumeTheWorld)
      pure e
    MinionMessage mid MinionSchemed | mid == minionId attrs -> do
      scheme <- selectOne (SideSchemeIs SideSchemes.consumeTheWorld)
      case scheme of
        Nothing -> DarkPhoenix <$> runMessage msg attrs
        Just sid -> do
          threat <- getModifiedScheme attrs
          placeThreatBy attrs W.ThreatFromMinion sid (threat + minionBoost attrs)
          checkWindows [W.Window W.After $ W.SchemedByMinion mid]
          pure $ DarkPhoenix attrs {minionBoost = 0}
    _ -> DarkPhoenix <$> runMessage msg attrs
