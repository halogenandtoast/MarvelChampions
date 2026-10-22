module Marvel.Event.Events.EarthsMightiestHeroes (
  earthsMightiestHeroes,
  EarthsMightiestHeroes (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Matchers qualified as M
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Ref
import Marvel.Trait (Trait (Avenger))

earthsMightiestHeroes :: EventCard EarthsMightiestHeroes
earthsMightiestHeroes = event EarthsMightiestHeroes Cards.earthsMightiestHeroes

newtype EarthsMightiestHeroes = EarthsMightiestHeroes (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Exhaust an Avenger character you control (its cost) → ready another Avenger
-- character you control.
instance RunMessage EarthsMightiestHeroes where
  runMessage msg e@(EarthsMightiestHeroes attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment _) | ident == eventId attrs -> do
      let exhausted = paymentExhaustedCharacters payment
      heroes <- selectList $ IdentityWithId you <> IdentityWithTrait Avenger <> M.ExhaustedIdentity
      allies <- selectList $ AllyControlledBy (IdentityWithId you) <> AllyWithTrait Avenger <> M.ExhaustedAlly
      -- none to ready: the event resolves to nothing
      let readied =
            [targeting (IdentityRef i) (ready i) | i <- heroes, IdentityCharacter i `notElem` exhausted]
              <> [targeting (AllyRef a) (ready a) | a <- allies, AllyCharacter a `notElem` exhausted]
      unless (null readied) $ chooseOrRunOne you readied
      pure e
    _ -> EarthsMightiestHeroes <$> runMessage msg attrs
