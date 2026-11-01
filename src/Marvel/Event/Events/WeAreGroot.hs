module Marvel.Event.Events.WeAreGroot (
  weAreGroot,
  WeAreGroot (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityCounters))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Status ()

weAreGroot :: EventCard WeAreGroot
weAreGroot = event WeAreGroot Cards.weAreGroot

newtype WeAreGroot = WeAreGroot (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: remove up to 4 growth counters from Groot → choose that many friendly
-- characters and give each a tough status card. The player picks how many (at most his
-- counters and the friendly characters there are), then that many different characters.
instance RunMessage WeAreGroot where
  runMessage msg e@(WeAreGroot attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      counters <- project PlayerIdentityCounters you
      characters <- length <$> selectList (CharacterControlledBy AnyIdentity)
      let most = minimum [4, counters, fromIntegral characters]
      when (most > 0) $
        chooseOne you
          [ labeled (countLabel "removeGrowthCounters" n) do
            removeCounters you n
            cardSpecific attrs "give" (you, n, [] :: [CharacterId])
          | n <- [1 .. most]
          ]
      pure e
    CardSpecific (isTarget attrs -> True) "give" (fromJSON -> Success (you, n :: Natural, given :: [CharacterId])) -> do
      characters <- filter (`notElem` given) <$> selectList (CharacterControlledBy AnyIdentity)
      when (n > 0) $
        chooseOnePrompted you "$label.giveTough"
          [ targeting (characterRef c) do
            giveStatus c ToughStatus
            cardSpecific attrs "give" (you, n - 1, c : given)
          | c <- characters
          ]
      pure e
    _ -> WeAreGroot <$> runMessage msg attrs
