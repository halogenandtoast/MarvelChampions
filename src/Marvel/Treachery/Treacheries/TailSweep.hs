module Marvel.Treachery.Treacheries.TailSweep (
  tailSweep,
  TailSweep (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

tailSweep :: TreacheryCard TailSweep
tailSweep = treachery TailSweep Cards.tailSweep

newtype TailSweep = TailSweep (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Scorpion attacks your hero; if no attack is made (no hero, no Scorpion, or a stunned
-- one, who only loses his stun), you are stunned.
instance RunMessage TailSweep where
  runMessage msg t@(TailSweep attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      isHero <- identityMatches HeroIdentity you
      mScorpion <- selectOne $ MinionIs Minions.scorpion
      case mScorpion of
        Just scorpion | isHero -> do
          stunned <- selectAny $ MinionWithId scorpion <> StunnedMinion
          enemyAttacks scorpion you
          when stunned $ stunIdentity you
        _ -> stunIdentity you
      pure t
    -- [star] Boost: you are stunned
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      stunIdentity you
      pure t
    _ -> TailSweep <$> runMessage msg attrs
