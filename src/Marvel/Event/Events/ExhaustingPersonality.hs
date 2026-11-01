module Marvel.Event.Events.ExhaustingPersonality where

import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Ref

exhaustingPersonality :: EventCard ExhaustingPersonality
exhaustingPersonality = event ExhaustingPersonality Cards.exhaustingPersonality

newtype ExhaustingPersonality = ExhaustingPersonality (Attrs Event)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- A cost alone is not a meaningful effect: the status option needs at least one
-- status it can add, and the draw option needs a token as well as a ready identity.
statusVillains :: HasGame m => m [VillainId]
statusVillains = do
  choose <- elem PlayersChooseTheVillain <$> getModifiers GameRef
  villains <- if choose then selectList AnyVillain else maybeToList <$> selectOne ActiveVillain
  filterM (\villain ->
    (||) <$> selectAny (EnemyWithId (EnemyVillainId villain) <> EnemyCanBeStunned)
         <*> selectAny (EnemyWithId (EnemyVillainId villain) <> EnemyCanBeConfused)) villains

personalityOptions :: HasGame m => m (Bool, Bool)
personalityOptions = do
  status <- (&&) <$> (notNull <$> statusVillains) <*> selectAny EncounterMainScheme
  tokens <- getMainSchemeTokenCount
  draw <- (tokens > 0 &&) <$> selectAny UnexhaustedIdentity
  pure (status, draw)

instance IsEvent ExhaustingPersonality where
  eventTargets _ = do
    (status, draw) <- personalityOptions
    pure $ if status || draw then Nothing else Just []

instance RunMessage ExhaustingPersonality where
  runMessage msg e@(ExhaustingPersonality attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      (status, draw) <- personalityOptions
      chooseOne you $
        [labeled "$label.deadpoolStunAndConfuse" (cardSpecific attrs "chooseStatusVillain" ()) | status]
          <> [ labeled "$label.deadpoolExhaustAndDraw" $
                 payCostThenWithPayment you attrs ExhaustAnyIdentityCost attrs "exhaustAndDraw" ()
             | draw
             ]
      pure e
    CardSpecific (isTarget attrs -> True) "chooseStatusVillain" _ -> do
      villains <- statusVillains
      unless (null villains) $ chooseOrRunOne (eventController attrs)
        [ targeting villain do
            continuation <- capture $ cardSpecific attrs "applyStatuses" villain
            payCostThen (eventController attrs) attrs MainSchemeAccelerationTokenCost continuation
        | villain <- villains
        ]
      pure e
    CardSpecific (isTarget attrs -> True) "applyStatuses" (fromJSON -> Success villain) -> do
      stun (villain :: VillainId) attrs
      confuse villain attrs
      pure e
    CardSpecific (isTarget attrs -> True) "exhaustAndDraw" (fromJSON -> Success ((), payment)) -> do
      tokens <- getMainSchemeTokenCount
      for_ [you | ExhaustedIdentityPayment you <- payments payment] $ \you -> drawCards you tokens
      pure e
    _ -> ExhaustingPersonality <$> runMessage msg attrs
