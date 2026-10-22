module Marvel.Event.Events.EarthsMightiestHeroes (
  earthsMightiestHeroes,
  EarthsMightiestHeroes (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Matchers qualified as M
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait (Trait (Avenger))

earthsMightiestHeroes :: EventCard EarthsMightiestHeroes
earthsMightiestHeroes = event EarthsMightiestHeroes Cards.earthsMightiestHeroes

newtype EarthsMightiestHeroes = EarthsMightiestHeroes (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Exhaust an Avenger character you control → ready another Avenger character you control.
instance RunMessage EarthsMightiestHeroes where
  runMessage msg e@(EarthsMightiestHeroes attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      heroes <- selectList $ IdentityWithId identityId <> IdentityWithTrait Avenger <> UnexhaustedIdentity
      allies <- selectList $ AllyControlledBy (IdentityWithId identityId) <> AllyWithTrait Avenger <> UnexhaustedAlly
      chooseOne identityId $
        [targeting (IdentityRef i) (exhaust i >> cardSpecific attrs "ready" (IdentityCharacter i)) | i <- heroes]
          <> [targeting (AllyRef a) (exhaust a >> cardSpecific attrs "ready" (AllyCharacter a)) | a <- allies]
      pure e
    CardSpecific (isTarget attrs -> True) "ready" (fromJSON -> Success (exhausted :: CharacterId)) -> do
      let you = eventController attrs
      heroes <- selectList $ IdentityWithId you <> IdentityWithTrait Avenger <> M.ExhaustedIdentity
      allies <- selectList $ AllyControlledBy (IdentityWithId you) <> AllyWithTrait Avenger <> M.ExhaustedAlly
      chooseOne you $
        [targeting (IdentityRef i) (ready i) | i <- heroes, IdentityCharacter i /= exhausted]
          <> [targeting (AllyRef a) (ready a) | a <- allies, AllyCharacter a /= exhausted]
      pure e
    _ -> EarthsMightiestHeroes <$> runMessage msg attrs
