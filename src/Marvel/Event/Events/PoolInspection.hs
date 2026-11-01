module Marvel.Event.Events.PoolInspection where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

newtype Meta = Meta {ignoringCrisis :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

poolInspection :: EventCard PoolInspection
poolInspection = event (PoolInspection . (`With` Meta True)) Cards.poolInspection

newtype PoolInspection = PoolInspection (Attrs Event `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsEvent PoolInspection where
  toEventAttrs (PoolInspection (attrs `With` _)) = attrs

instance IsRef PoolInspection where
  toRef = toRef . toEventAttrs

instance HasModifiersFor PoolInspection where
  getModifiersFor _ target (PoolInspection (attrs `With` meta)) =
    pure [IgnoresCrisis | target == toRef attrs && ignoringCrisis meta]

instance RunMessage PoolInspection where
  runMessage msg e@(PoolInspection (attrs `With` meta)) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      schemes <- selectList (MainScheme <> ThwartableSchemeIgnoringCrisis)
      for_ schemes $ \scheme -> thwartSchemeAs you scheme attrs 5
      cardSpecific attrs "ordinaryCrisisRules" ()
      pure e
    CardSpecific (isTarget attrs -> True) "ordinaryCrisisRules" _ -> do
      cardSpecific attrs "each" ()
      pure $ PoolInspection (attrs `With` Meta False)
    CardSpecific (isTarget attrs -> True) "each" _ -> do
      amount <- getEncounterIconCount
      schemes <- selectList ThwartableScheme
      thwartEachAs (eventController attrs) attrs [(scheme, amount) | scheme <- schemes]
      pure e
    _ -> PoolInspection . (`With` meta) <$> runMessage msg attrs
