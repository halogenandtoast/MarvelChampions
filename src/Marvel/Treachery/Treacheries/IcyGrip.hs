module Marvel.Treachery.Treacheries.IcyGrip (
  icyGrip,
  IcyGrip (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

icyGrip :: TreacheryCard IcyGrip
icyGrip = treachery IcyGrip Cards.icyGrip

newtype IcyGrip = IcyGrip (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | You are stunned; if Absorbing Man has the Ice trait, take 2 indirect damage.
-- [star] Boost: if he has the Ice or Metal trait, give him a tough status card.
instance RunMessage IcyGrip where
  runMessage msg t@(IcyGrip attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      stunIdentity you
      whenM (selectAny $ VillainWithTrait Ice) $ dealIndirectDamage you attrs 2
      pure t
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      villain <- selectOne ActiveVillain
      for_ villain \vid -> do
        iceOrMetal <- orM [villainMatches (VillainWithTrait Ice) vid, villainMatches (VillainWithTrait Metal) vid]
        when iceOrMetal $ enemyBecomesTough vid
      pure t
    _ -> IcyGrip <$> runMessage msg attrs
