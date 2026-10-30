module Marvel.Treachery.Treacheries.SmVenom where

import Marvel.Prelude
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Environment.Cards qualified as Environments
import Marvel.Environment.Environments.SmVenom (removeChimes)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

newtype VenomTreacheryMeta = VenomTreacheryMeta {venomActivationBonus :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype SmVenomTreachery = SmVenomTreachery (Attrs Treachery `With` VenomTreacheryMeta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsTreachery SmVenomTreachery where
  toTreacheryAttrs (SmVenomTreachery (attrs `With` _)) = attrs
  treacheryModifiersFor _ GameRef (SmVenomTreachery (_ `With` meta)) = pure [AdditionalActivationBoostIcons 1 | venomActivationBonus meta]
  treacheryModifiersFor _ _ _ = pure []

venomTreachery :: Attrs Treachery -> SmVenomTreachery
venomTreachery = SmVenomTreachery . (`With` VenomTreacheryMeta False)

smBitingRetort, smForWhomTheBellTolls :: TreacheryCard SmVenomTreachery
smBitingRetort = treachery venomTreachery Cards.smBitingRetort
smForWhomTheBellTolls = treachery venomTreachery Cards.smForWhomTheBellTolls

instance RunMessage SmVenomTreachery where
  runMessage msg e@(SmVenomTreachery (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> reveal you
    CardSpecific (isTarget attrs -> True) "endActivationBonus" _ -> pure $ SmVenomTreachery (attrs `With` VenomTreacheryMeta False)
    CardSpecific (isTarget attrs -> True) "afterChimes" (fromJSON -> Success you) -> do
      isQuiet <- selectAny (EnvironmentIs Environments.smBellTowerA)
      isRinging <- selectAny (EnvironmentIs Environments.smBellTowerB)
      when isQuiet $ damageIdentity you attrs (toDamage 1 FromAbility)
      when isRinging $ removeThreat you attrs 1 EncounterMainScheme
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      if toCardCode attrs == "27082" then e <$ removeChimes 1 else reveal you
    _ -> SmVenomTreachery . (`With` meta) <$> runMessage msg attrs
   where
    reveal you
      | toCardCode attrs == "27082" = do
          villain <- selectOne ActiveVillain
          for_ villain (`villainActivatesAgainst` you)
          cardSpecific attrs "endActivationBonus" ()
          pure $ SmVenomTreachery (attrs `With` VenomTreacheryMeta True)
      | otherwise = do
          removeChimes 2
          cardSpecific attrs "afterChimes" (you :: IdentityId)
          pure e
