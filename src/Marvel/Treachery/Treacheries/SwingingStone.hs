module Marvel.Treachery.Treacheries.SwingingStone (
  swingingStone,
  SwingingStone (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Trait
import Marvel.Ref (IsRef)
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

swingingStone :: TreacheryCard SwingingStone
swingingStone = treachery SwingingStone Cards.swingingStone

newtype SwingingStone = SwingingStone (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Alter-ego: Absorbing Man schemes; hero: he attacks you. With the Stone trait he gets
-- +1 SCH or ATK for this activation.
instance RunMessage SwingingStone where
  runMessage msg t@(SwingingStone attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      villain <- selectOne ActiveVillain
      for_ villain \vid -> do
        hero <- identityMatches HeroIdentity you
        stone <- villainMatches (VillainWithTrait Stone) vid
        when stone $ activationBonus vid 1
        if hero then enemyAttacks vid you else villainSchemes vid
      pure t
    _ -> SwingingStone <$> runMessage msg attrs
