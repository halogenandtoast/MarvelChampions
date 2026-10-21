module Marvel.Treachery.Treacheries.ConcussiveBlast (
  concussiveBlast,
  ConcussiveBlast (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

concussiveBlast :: TreacheryCard ConcussiveBlast
concussiveBlast = treachery ConcussiveBlast Cards.concussiveBlast

newtype ConcussiveBlast = ConcussiveBlast (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ConcussiveBlast where
  runMessage msg t@(ConcussiveBlast attrs) = case msg of
    -- 1 damage to each friendly character
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      damageEach AnyIdentity AnyAlly
      pure t
    -- Boost: 1 damage to each character you control
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      ident <- getActivePlayerId
      damageEach (IdentityWithId ident) (AllyControlledBy $ IdentityWithId ident)
      pure t
    _ -> ConcussiveBlast <$> runMessage msg attrs
   where
    damage = toDamage 1 FromTreachery
    damageEach identities allies = do
      players <- selectList identities
      allyIds <- selectList allies
      pushAll $
        [IdentityMessage p (IdentityDamaged (toSource attrs) damage) | p <- players]
          <> [AllyMessage a (AllyDamaged (toSource attrs) damage) | a <- allyIds]
