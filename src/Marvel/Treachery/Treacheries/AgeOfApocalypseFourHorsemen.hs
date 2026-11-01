module Marvel.Treachery.Treacheries.AgeOfApocalypseFourHorsemen where

import Marvel.Prelude
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Query
import Marvel.Scenario.AgeOfApocalypseFourHorsemen
import Marvel.Status
import Marvel.Card
import Marvel.Entity
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype AoaFourHorsemenTreachery = AoaFourHorsemenTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

horsemanOfWar :: TreacheryCard AoaFourHorsemenTreachery
horsemanOfWar = treachery AoaFourHorsemenTreachery Cards.horsemanOfWar

horsemanOfFamine :: TreacheryCard AoaFourHorsemenTreachery
horsemanOfFamine = treachery AoaFourHorsemenTreachery Cards.horsemanOfFamine

horsemanOfPestilence :: TreacheryCard AoaFourHorsemenTreachery
horsemanOfPestilence = treachery AoaFourHorsemenTreachery Cards.horsemanOfPestilence

horsemanOfDeath :: TreacheryCard AoaFourHorsemenTreachery
horsemanOfDeath = treachery AoaFourHorsemenTreachery Cards.horsemanOfDeath

roughRiders :: TreacheryCard AoaFourHorsemenTreachery
roughRiders = treachery AoaFourHorsemenTreachery Cards.roughRiders


instance RunMessage AoaFourHorsemenTreachery where
  runMessage msg a@(AoaFourHorsemenTreachery attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      if toCardCode attrs == "45096" then cardSpecific attrs "roughRidersFirst" you
      else do
        named <- findHorseman $ horsemanName $ toCardCode attrs
        for_ named $ \vid -> do
          heal vid 2
          giveStatus vid ToughStatus
          villainActivatesAgainst vid you
      pure a
    CardSpecific (isTarget attrs -> True) "roughRidersFirst" (fromJSON -> Success you) -> do
      active <- selectOne ActiveVillain
      for_ active $ \vid -> horsemanResponse vid you
      moveHorsemanCounter
      cardSpecific attrs "roughRidersSecond" you
      pure a
    CardSpecific (isTarget attrs -> True) "roughRidersSecond" (fromJSON -> Success you) -> do
      active <- selectOne ActiveVillain
      for_ active $ \vid -> horsemanResponse vid you
      pure a
    Boost (RevealedAsBoost target enemy) | isTarget attrs target, toCardCode attrs /= "45096" -> do
      you <- getBoostedPlayer enemy
      named <- findHorseman $ horsemanName $ toCardCode attrs
      for_ named $ \vid -> enemyActivationAfterCurrent enemy $ do
        hero <- identityMatches HeroIdentity you
        if hero then villainAttacksWithoutBoost vid you else villainSchemesWithoutBoostAgainst vid you
      pure a
    _ -> AoaFourHorsemenTreachery <$> runMessage msg attrs

horsemanName :: CardCode -> Text
horsemanName = \case
  "45092" -> "War"
  "45093" -> "Famine"
  "45094" -> "Pestilence"
  "45095" -> "Death"
  _ -> ""
