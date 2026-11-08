module Marvel.Event.Events.UnitedWeStand (
  unitedWeStand,
  UnitedWeStand (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source (getVillainStage)
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

unitedWeStand :: EventCard UnitedWeStand
unitedWeStand = event UnitedWeStand Cards.unitedWeStand

newtype UnitedWeStand = UnitedWeStand (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Play only if your identity has the Avenger trait (its criteria). "Hero Action: Heal
-- 1 damage from up to X friendly characters (to a maximum of 3 characters), where X is
-- equal to the villain's stage number." One at a time, each a different damaged
-- character, until the player is done.
instance RunMessage UnitedWeStand where
  runMessage msg e@(UnitedWeStand attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      stage <- maybe (pure 0) getVillainStage =<< selectOne ActiveVillain
      cardSpecific attrs "heal" (you, min 3 stage, [] :: [CharacterId])
      pure e
    CardSpecific (isTarget attrs -> True) "heal" (fromJSON -> Success (you, n :: Natural, healed :: [CharacterId])) -> do
      characters <- filter (`notElem` healed) <$> selectList (CharacterControlledBy AnyIdentity <> healableCharacter)
      when (n > 0 && notNull characters) $
        chooseOnePrompted you (countLabel "healAFriendlyCharacter" 1) $
          labeled "$label.done" (pure ())
            : [ targeting (characterRef c) do
                  heal c 1
                  cardSpecific attrs "heal" (you, n - 1, c : healed)
              | c <- characters
              ]
      pure e
    _ -> UnitedWeStand <$> runMessage msg attrs
