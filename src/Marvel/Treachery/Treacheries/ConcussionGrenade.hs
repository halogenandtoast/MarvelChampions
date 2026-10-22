module Marvel.Treachery.Treacheries.ConcussionGrenade (
  concussionGrenade,
  ConcussionGrenade (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Ref (IsRef)
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

concussionGrenade :: TreacheryCard ConcussionGrenade
concussionGrenade = treachery ConcussionGrenade Cards.concussionGrenade

newtype ConcussionGrenade = ConcussionGrenade (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Alter-ego: you are confused, and 1 threat goes on the main scheme (2 if you were
-- already confused). Hero: you are stunned, and your hero takes 1 damage (2 if you were
-- already stunned).
instance RunMessage ConcussionGrenade where
  runMessage msg t@(ConcussionGrenade attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      hero <- identityMatches HeroIdentity you
      if hero
        then do
          already <- identityMatches StunnedIdentity you
          stunIdentity you
          damageIdentity you attrs (toDamage (if already then 2 else 1) FromTreachery)
        else do
          already <- identityMatches ConfusedIdentity you
          confuseIdentity you
          placeThreatOnEach attrs (if already then 2 else 1) MainScheme
      pure t
    _ -> ConcussionGrenade <$> runMessage msg attrs
