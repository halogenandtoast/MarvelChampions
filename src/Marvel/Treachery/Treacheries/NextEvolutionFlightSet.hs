module Marvel.Treachery.Treacheries.NextEvolutionFlightSet where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword qualified as Keyword
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Status
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W ()
import Marvel.NextEvolution.Sinister ()
import Marvel.Card
import Marvel.Entity
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Message.Lifted

newtype FlightSetTreachery = FlightSetTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40154 :: TreacheryCard FlightSetTreachery
ne40154 = treachery FlightSetTreachery Cards.ne40154

instance RunMessage FlightSetTreachery where
  runMessage msg card@(FlightSetTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery _) | tid == treacheryId a -> do
      characters <- selectList (CharacterControlledBy AnyIdentity <> CharacterWithStatus ToughStatus)
      for_ characters removeAllTough
      brute <- selectAny (ActiveVillain <> VillainWithTrait Trait.Brute)
      dealIndirectDamageAmongPlayers a (if brute then 4 else 2)
      pure card
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      case enemy of
        EnemyVillainId vid -> whenM (isJust <$> getVillainAttack vid) $ villainStep vid (VillainAttackGains Keyword.Piercing)
        _ -> pure ()
      pure card
    _ -> FlightSetTreachery <$> runMessage msg a
