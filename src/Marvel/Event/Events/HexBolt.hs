module Marvel.Event.Events.HexBolt (
  hexBolt,
  HexBolt (..),
) where

import Marvel.Prelude

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
-- the player's chosen order, each a step of its own so it sees prior effects.
instance RunMessage HexBolt where
  runMessage msg e@(HexBolt attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent {}) | ident == eventId attrs -> do
      discardTopOfEncounterDeckThen 3 (countBoostIcons attrs ())
      pure e
    WithBoostIcons (isTarget attrs -> True) _ (map snd -> counts) -> do
      chooseOneAtATimePrompted you "$label.hexBoltChooseEffect"
        [labeled (bulletLabel k) $ cardSpecific attrs "bullet" k | k <- map (min 3) counts]
      pure e
    CardSpecific (isTarget attrs -> True) "bullet" (fromJSON -> Success (k :: Natural)) -> do
      case k of
        0 -> chooseDamage you attrs (toDamage 2 FromAbility) AnyEnemy
        1 -> removeThreat you attrs 2 ThwartableScheme
        2 -> drawCards you 1
        _ -> do
          characters <- filterM (fmap notNull . lacking) =<< selectList AnyCharacter
          unless (null characters) $ chooseOnePromptedAbout you "$label.hexBoltChooseCharacter" attrs
            [targeting (characterRef c) $ cardSpecific attrs "status" c | c <- characters]
      pure e
    CardSpecific (isTarget attrs -> True) "status" (fromJSON -> Success (c :: CharacterId)) -> do
      statuses <- lacking c
      unless (null statuses) $ chooseOnePromptedAbout you "$label.hexBoltChooseStatus" (characterRef c)
        [labeled (statusLabel s) (giveStatus c s) | s <- statuses]
      pure e
    _ -> HexBolt <$> runMessage msg attrs
   where
    you = eventController attrs
    bulletLabel = \case
      0 -> countLabel "dealDamageToAnEnemy" 2
      1 -> countLabel "removeThreatFromAScheme" 2
      2 -> "$label.drawOneCard"
      _ -> "$label.hexBoltChooseCharacter"
    -- a character has one status card of each type at most
    lacking c = filterM (\s -> not . member c <$> select (CharacterWithStatus s)) [minBound .. maxBound]
