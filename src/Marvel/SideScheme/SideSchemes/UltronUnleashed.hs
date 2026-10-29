module Marvel.SideScheme.SideSchemes.UltronUnleashed where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Query
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Window qualified as W
import Marvel.GameValue
ultronUnleashed :: SideSchemeCard UltronUnleashed
ultronUnleashed = sideScheme UltronUnleashed Cards.ultronUnleashed (PerPlayer 3)

newtype UltronUnleashed = UltronUnleashed (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage UltronUnleashed where
  runMessage msg s@(UltronUnleashed attrs) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      you <- getYou
      searchEverywhereAndPutIntoPlayEncounterCard you (CardWithTitle "Ultron Drones")
      cardSpecific attrs "dronesForEveryone" ()
      pure s
    CardSpecific (isTarget attrs -> True) "dronesForEveryone" _ -> do
      players <- getPlayers
      for_ players putTopOfDeckIntoPlayAsDrone
      pure s
    _ -> UltronUnleashed <$> runMessage msg attrs
