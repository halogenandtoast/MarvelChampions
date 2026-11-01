module Marvel.Event.Events.EvenTheOdds where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card ()
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Damage
import Marvel.Id ()
import Marvel.Game.Source (getStartingPlayerCount)

-- Count schemes actually defeated by this thwart, after interrupts and replacement effects.
evenTheOdds :: EventCard EvenTheOdds
evenTheOdds = event (EvenTheOdds . (`With` (0 :: Natural))) Cards.evenTheOdds

newtype EvenTheOdds = EvenTheOdds (Attrs Event `With` Natural)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsEvent EvenTheOdds where
  toEventAttrs (EvenTheOdds (attrs `With` _)) = attrs

instance IsRef EvenTheOdds where
  toRef = toRef . toEventAttrs

instance HasModifiersFor EvenTheOdds

instance RunMessage EvenTheOdds where
  runMessage msg e@(EvenTheOdds (attrs `With` defeated)) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ _ _) | eid == eventId attrs -> do
      schemes <- selectList AnySideScheme
      n <- fromIntegral <$> getStartingPlayerCount
      thwartEach attrs [(sid,n) | sid <- schemes]
      cardSpecific attrs "damage" ()
      pure e
    CheckWindows windows -> do
      let count = fromIntegral $ length [() | W.Window W.After (W.DefeatedSideSchemeBy _ _ (Just source)) <- windows, source == toSource attrs]
      attrs' <- runMessage msg attrs
      pure $ EvenTheOdds (attrs' `With` (defeated + count))
    CardSpecific (isTarget attrs -> True) "damage" _ -> do
      when (defeated > 0) $ withTheVillain (eventController attrs) $ \vid -> dealDamage vid attrs (toDamage defeated FromAbility)
      pure e
    _ -> EvenTheOdds . (`With` defeated) <$> runMessage msg attrs
