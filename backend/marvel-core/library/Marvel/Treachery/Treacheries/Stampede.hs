module Marvel.Treachery.Treacheries.Stampede
  ( stampede
  , Stampede(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

stampede :: TreacheryCard Stampede
stampede = treachery Stampede Cards.stampede

newtype Stampede = Stampede (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage Stampede where
  runMessage msg t@(Stampede attrs) = case msg of
    TreacheryMessage ident msg' | ident == treacheryId attrs -> case msg' of
      RevealTreachery identityId -> do
        isHero <- identityMatches HeroIdentity identityId
        if isHero
          then do
            villainId <- selectJust ActiveVillain
            push $ VillainMessage villainId $ VillainAttacks identityId
            pure t
          else pure . Stampede $ attrs & surgeL .~ True
      _ -> Stampede <$> runMessage msg attrs
    AllyMessage aid (AllyDamaged _ _) -> do
      push $ AllyMessage aid AllyStunned
      pure t
    IdentityMessage aid (IdentityDamaged _ _) -> do
      push $ IdentityMessage aid IdentityStunned
      pure t
    _ -> Stampede <$> runMessage msg attrs
