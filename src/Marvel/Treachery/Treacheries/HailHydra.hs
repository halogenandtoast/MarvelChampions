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
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

hailHydra :: TreacheryCard HailHydra
hailHydra = treachery (HailHydra . (`With` Meta [])) Cards.hailHydra

-- | The players a Hydra minion's attack began against.
newtype Meta = Meta {attacked :: [IdentityId]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Show, Eq)

newtype HailHydra = HailHydra (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery HailHydra where
  toTreacheryAttrs (HailHydra (attrs `With` _)) = attrs

-- | Each Hydra minion engaged with a hero attacks them; a stunned one does not, its stun
-- removed instead, and an attack cancelled is not made. Each player whom no attack was
-- made against puts a Hydra minion from the encounter deck or discard pile into play
-- engaged with them.
instance RunMessage HailHydra where
  runMessage msg t@(HailHydra (attrs `With` meta)) = case msg of
    TreacheryMessage ident (RevealTreachery _) | treacheryId attrs == ident -> do
      players <- getPlayers
      attacks <- for players \p -> do
        isHero <- identityMatches HeroIdentity p
        minions <- selectList $ MinionEngagedWith (IdentityWithId p) <> MinionWithTrait Hydra
        pure [MinionMessage m (MinionAttacks p) | isHero, m <- minions]
      pushAll $ concat attacks <> [cardSpecific attrs "search" ()]
      pure t
    MinionMessage mid (MinionBeginAttack p) -> do
      hydra <- selectAny (MinionWithId mid <> MinionWithTrait Hydra)
      pure $ if hydra then HailHydra (attrs `With` Meta (p : attacked meta)) else t
    CardSpecific (isTarget attrs -> True) "search" _ -> do
      players <- getPlayers
      pushAll
        [ SearchForAndPutIntoPlayEncounterCard p (CardWithType MinionType <> CardWithTrait Hydra)
        | p <- players
        , p `notElem` attacked meta
        ]
      pure t
    _ -> HailHydra . (`With` meta) <$> runMessage msg attrs
