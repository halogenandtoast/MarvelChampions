module Marvel.Event.Events.NoQuarter where

import Marvel.Prelude
import Marvel.Aspect
import Marvel.Card.Types (Card (..))
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

newtype NoQuarterMeta = NoQuarterMeta {excessResolved :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype NoQuarter = NoQuarter (Attrs Event `With` NoQuarterMeta)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

noQuarter :: EventCard NoQuarter
noQuarter = event (NoQuarter . (`With` NoQuarterMeta False)) Cards.noQuarter

instance IsEvent NoQuarter where
  toEventAttrs (NoQuarter (attrs `With` _)) = attrs

instance RunMessage NoQuarter where
  runMessage msg e@(NoQuarter (attrs `With` meta)) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      chooseDamage you attrs (toDamage 4 $ FromPlayerAttack you) AttackableEnemy
      pure e
    CheckWindows windows
      | n : _ <- [excess | W.Window _ window <- windows, (source, excess) <- defeat window, source == toSource attrs, excess > 0]
      , not (excessResolved meta) -> do
          discardFromDeck (eventController attrs) n attrs
          attrs' <- runMessage msg attrs
          pure $ NoQuarter (attrs' `With` NoQuarterMeta True)
    WithDiscarded (isTarget attrs -> True) FromDeck cards -> do
      for_ (filter (cardMatch $ CardWithAspect Aggression) cards) $ \card -> case card of
        PlayerCard playerCard -> addToHand (eventController attrs) playerCard
        _ -> pure ()
      pure e
    _ -> NoQuarter . (`With` meta) <$> runMessage msg attrs
   where
    defeat = \case
      W.DefeatedMinion _ _ source excess -> [(source, excess)]
      W.DefeatedVillain _ _ source excess -> [(source, excess)]
      _ -> []
