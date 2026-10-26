module Marvel.Event.Events.HexBolt (
  hexBolt,
  HexBolt (..),
) where

import Marvel.Prelude

import Data.List (sort)

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

hexBolt :: EventCard HexBolt
hexBolt = event HexBolt Cards.hexBolt

newtype HexBolt = HexBolt (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | All 3 cards are discarded first, then counted (FAQ); each card's bullet resolves in
-- the order written, each a step of its own so it sees the game the one before left.
instance RunMessage HexBolt where
  runMessage msg e@(HexBolt attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent {}) | ident == eventId attrs -> do
      discardTopOfEncounterDeckThen 3 (countBoostIcons attrs ())
      pure e
    WithBoostIcons (isTarget attrs -> True) _ (map snd -> counts) -> do
      for_ (sort $ map (min 3) counts) $ cardSpecific attrs "bullet"
      pure e
    CardSpecific (isTarget attrs -> True) "bullet" (fromJSON -> Success (k :: Natural)) -> do
      case k of
        0 -> chooseDamage you attrs (toDamage 2 FromAbility) AnyEnemy
        1 -> removeThreat you attrs 2 ThwartableScheme
        2 -> drawCards you 1
        _ -> do
          characters <- filterM (fmap notNull . lacking) =<< selectList AnyCharacter
          chooseOne you [targeting (characterRef c) $ cardSpecific attrs "status" c | c <- characters]
      pure e
    CardSpecific (isTarget attrs -> True) "status" (fromJSON -> Success (c :: CharacterId)) -> do
      statuses <- lacking c
      chooseOne you [labeled (statusLabel s) (giveStatus c s) | s <- statuses]
      pure e
    _ -> HexBolt <$> runMessage msg attrs
   where
    you = eventController attrs
    -- a character has one status card of each type at most
    lacking c = filterM (\s -> not . member c <$> select (CharacterWithStatus s)) [minBound .. maxBound]
