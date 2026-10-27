module Marvel.Event.Events.FloraAndFauna (
  floraAndFauna,
  FloraAndFauna (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

floraAndFauna :: EventCard FloraAndFauna
floraAndFauna = event FloraAndFauna Cards.floraAndFauna

newtype FloraAndFauna = FloraAndFauna (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Place 2 growth counters on Groot (to a maximum of 10) and ready him, or
-- place 2 charge counters on a Rocket Raccoon upgrade and ready that upgrade. "Groot" is
-- a friendly character titled Groot: his identity, or a Groot ally (readied only, as an
-- ally has no growth counters). A Rocket Raccoon upgrade is a hero-specific upgrade a
-- Rocket Raccoon player controls; its charge counters are its uses.
instance RunMessage FloraAndFauna where
  runMessage msg e@(FloraAndFauna attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      groots <- selectList (IdentityWithTitle "Groot")
      grootAllies <- selectList (AllyWithTitle "Groot")
      rocketUpgrades <-
        selectList $ UpgradeControlledBy (IdentityWithTitle "Rocket Raccoon") <> UpgradeWithCard HeroSpecificCard
      chooseOne you $
        [targeting (groot :: IdentityId) (placeGrowthCounters groot 2 >> ready groot) | groot <- groots]
          <> [targeting (grootAlly :: AllyId) (ready grootAlly) | grootAlly <- grootAllies]
          <> [targeting (u :: UpgradeId) (addUpgradeUses u 2 >> ready u) | u <- rocketUpgrades]
      pure e
    _ -> FloraAndFauna <$> runMessage msg attrs
