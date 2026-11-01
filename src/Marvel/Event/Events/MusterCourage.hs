module Marvel.Event.Events.MusterCourage (
  musterCourage,
  MusterCourage (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Game.Source (getVillainStage)
import Marvel.Status ()

musterCourage :: EventCard MusterCourage
musterCourage = event MusterCourage Cards.musterCourage

newtype MusterCourage = MusterCourage (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Give up to X friendly characters a tough status card (to a maximum of 3 characters),
-- where X is equal to the villain's stage number. One at a time, until the player is done.
instance RunMessage MusterCourage where
  runMessage msg e@(MusterCourage attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      stage <- maybe (pure 0) getVillainStage =<< selectOne ActiveVillain
      cardSpecific attrs "give" (you, min 3 stage, [] :: [CharacterId])
      pure e
    CardSpecific (isTarget attrs -> True) "give" (fromJSON -> Success (you, n :: Natural, given :: [CharacterId])) -> do
      characters <- filter (`notElem` given) <$> selectList (CharacterControlledBy AnyIdentity)
      when (n > 0 && notNull characters) $
        chooseOnePrompted you "$label.giveTough" $
          labeled "$label.done" (pure ())
            : [ targeting (characterRef c) do
                  giveStatus c ToughStatus
                  cardSpecific attrs "give" (you, n - 1, c : given)
              | c <- characters
              ]
      pure e
    _ -> MusterCourage <$> runMessage msg attrs
