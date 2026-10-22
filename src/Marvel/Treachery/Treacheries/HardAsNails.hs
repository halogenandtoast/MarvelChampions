module Marvel.Treachery.Treacheries.HardAsNails (
  hardAsNails,
  HardAsNails (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

hardAsNails :: TreacheryCard HardAsNails
hardAsNails = treachery HardAsNails Cards.hardAsNails

newtype HardAsNails = HardAsNails (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Give the villain a tough status card; one already tough cannot be given another,
-- and heals 3 damage instead.
toughenVillain :: (ReverseQueue m) => m ()
toughenVillain = do
  villain <- selectOne ActiveVillain
  for_ villain \vid -> do
    tough <- villainMatches VillainWithToughStatus vid
    if tough then heal vid 3 else enemyBecomesTough vid

-- | When Revealed and [star] Boost alike.
instance RunMessage HardAsNails where
  runMessage msg t@(HardAsNails attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> t <$ toughenVillain
    Boost (RevealedAsBoost target _) | isTarget attrs target -> t <$ toughenVillain
    _ -> HardAsNails <$> runMessage msg attrs
