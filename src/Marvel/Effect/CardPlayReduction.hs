-- | A reduction scoped to the nested play of one physical card. The game queues
-- its removal after that play returns, including aborted payments.
module Marvel.Effect.CardPlayReduction (CardPlayReduction (..)) where

import Marvel.Prelude
import Marvel.Effect.Types
import Marvel.Entity ()
import Marvel.Ref
import Marvel.Message.Lifted

newtype CardPlayReduction = CardPlayReduction (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage CardPlayReduction where
  runMessage msg e@(CardPlayReduction attrs) = runQueueT $ case msg of
    IdentityMessage _ (PlayedCard card _) | toRef card == effectSource attrs -> do
      disableEffect (effectId attrs)
      pure e
    IdentityMessage _ (AddToHand card) | toRef card == effectSource attrs -> e <$ disableEffect (effectId attrs)
    _ -> CardPlayReduction <$> runMessage msg attrs
