module Marvel.SideScheme.SideSchemes.SeekingVengeance where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Status ()
import Marvel.Window qualified as W ()
import Marvel.GameValue
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

seekingVengeance :: SideSchemeCard SeekingVengeance
seekingVengeance = sideScheme SeekingVengeance Cards.seekingVengeance (Static 5)

newtype SeekingVengeance = SeekingVengeance (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SeekingVengeance where
  runMessage msg (SeekingVengeance attrs) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      you <- getYou
      selectOne (MinionIs Minions.ladyDeathstrike) >>= \case
        Nothing -> searchAndPutIntoPlayEncounterCard you (CardIs Minions.ladyDeathstrike)
        Just mid -> enemyActivationAfterCurrent mid do
          hero <- identityMatches HeroIdentity you
          if hero then enemyAttacks mid you else minionSchemesAgainst mid you
      SeekingVengeance <$> runMessage msg attrs
    _ -> SeekingVengeance <$> runMessage msg attrs
