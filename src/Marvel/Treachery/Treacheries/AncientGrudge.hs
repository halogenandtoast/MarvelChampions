module Marvel.Treachery.Treacheries.AncientGrudge (
  ancientGrudge,
  AncientGrudge (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

ancientGrudge :: TreacheryCard AncientGrudge
ancientGrudge = treachery AncientGrudge Cards.ancientGrudge

newtype AncientGrudge = AncientGrudge (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Kang (Master of Time) activates against you: he attacks your hero, or schemes
-- against your alter-ego. Not in play, he is found in the encounter deck or discard pile
-- and put into play engaged with you, the deck shuffled.
instance RunMessage AncientGrudge where
  runMessage msg t@(AncientGrudge attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      mKang <- selectOne $ MinionIs Minions.kangMasterOfTime
      case mKang of
        Just kang -> do
          isHero <- identityMatches HeroIdentity you
          if isHero then enemyAttacks kang you else minionSchemes kang
        Nothing -> searchAndPutIntoPlayEncounterCard you (CardIs Minions.kangMasterOfTime)
      pure t
    _ -> AncientGrudge <$> runMessage msg attrs
