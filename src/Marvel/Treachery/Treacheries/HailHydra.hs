module Marvel.Treachery.Treacheries.HailHydra (
  hailHydra,
  HailHydra (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.Types
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

hailHydra :: TreacheryCard HailHydra
hailHydra = treachery HailHydra Cards.hailHydra

newtype HailHydra = HailHydra (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Each Hydra minion engaged with a hero attacks them; a stunned one does not, its stun
-- removed instead. Each player not attacked puts a Hydra minion from the encounter deck
-- or discard pile into play engaged with them.
instance RunMessage HailHydra where
  runMessage msg t@(HailHydra attrs) = case msg of
    TreacheryMessage ident (RevealTreachery _) | treacheryId attrs == ident -> do
      players <- getPlayers
      attacks <- for players \p -> do
        isHero <- identityMatches HeroIdentity p
        minions <- selectList $ MinionEngagedWith (IdentityWithId p) <> MinionWithTrait Hydra
        attackers <- selectList $ MinionEngagedWith (IdentityWithId p) <> MinionWithTrait Hydra <> NotMinion StunnedMinion
        pure (p, if isHero then minions else [], isHero && notNull attackers)
      pushAll $
        [MinionMessage m (MinionAttacks p) | (p, minions, _) <- attacks, m <- minions]
          <> [ SearchForAndPutIntoPlayEncounterCard p (CardWithType MinionType <> CardWithTrait Hydra)
             | (p, _, False) <- attacks
             ]
      pure t
    _ -> HailHydra <$> runMessage msg attrs
