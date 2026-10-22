module Marvel.Treachery.Treacheries.BitterRival (
  bitterRival,
  BitterRival (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

bitterRival :: TreacheryCard BitterRival
bitterRival = treachery BitterRival Cards.bitterRival

newtype BitterRival = BitterRival (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The player chooses a ready character they control and exhausts it, then does what
-- follows; with none ready, nothing is exhausted.
exhaustACharacter :: (ReverseQueue m) => IdentityId -> m () -> m ()
exhaustACharacter you andThen = do
  ready <- identityMatches UnexhaustedIdentity you
  allies <- selectList (UnexhaustedAlly <> AllyControlledBy (IdentityWithId you))
  let choices =
        [targeting you (exhaust you >> andThen) | ready]
          <> [targeting ally (exhaust ally >> andThen) | ally <- allies]
  unless (null choices) $ chooseOne you choices

-- | When Revealed (errata): for each side scheme in play, choose and exhaust a character
-- you control. Boost: exhaust a character you control.
instance RunMessage BitterRival where
  runMessage msg t@(BitterRival attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      n <- selectListCount AnySideScheme
      cardSpecific attrs "exhaust" (you, n)
      pure t
    CardSpecific (isTarget attrs -> True) "exhaust" (fromJSON -> Success (you, n :: Natural)) -> do
      when (n > 0) $ exhaustACharacter you (cardSpecific attrs "exhaust" (you, n - 1))
      pure t
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      you <- getActivePlayerId
      exhaustACharacter you (pure ())
      pure t
    _ -> BitterRival <$> runMessage msg attrs
