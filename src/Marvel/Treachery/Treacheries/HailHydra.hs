module Marvel.Treachery.Treacheries.HailHydra (
  hailHydra,
  HailHydra (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Id
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

hailHydra :: TreacheryCard HailHydra
hailHydra = treachery (HailHydra . (`With` Meta [])) Cards.hailHydra

-- | The players attacked by its Hydra minions.
newtype Meta = Meta {attacked :: [IdentityId]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Show, Eq)

newtype HailHydra = HailHydra (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery HailHydra where
  toTreacheryAttrs (HailHydra (attrs `With` _)) = attrs

-- | Each Hydra minion engaged with a hero attacks them; a stunned one does not, its stun
-- removed instead. Then each player who was not attacked this way puts a Hydra minion
-- of their choice from the encounter deck or discard pile into play engaged with them.
instance RunMessage HailHydra where
  runMessage msg t@(HailHydra (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | treacheryId attrs == ident -> do
      players <- getPlayers
      heroes <- filterM (identityMatches HeroIdentity) players
      for_ heroes \p -> do
        minions <- selectList $ MinionEngagedWith (IdentityWithId p) <> MinionWithTrait Hydra
        for_ minions (`enemyAttacks` p)
      cardSpecific attrs "search" ()
      pure t
    MinionMessage mid MinionAttacked -> do
      hydra <- minionMatches (MinionWithTrait Hydra) mid
      engaged <- selectList $ IdentityEngagedWith (MinionWithId mid)
      pure $ if hydra then HailHydra (attrs `With` Meta (attacked meta <> engaged)) else t
    CardSpecific (isTarget attrs -> True) "search" _ -> do
      players <- getPlayers
      for_ [p | p <- players, p `notElem` attacked meta] \p ->
        searchAndPutIntoPlayEncounterCard p (CardWithType MinionType <> CardWithTrait Hydra)
      pure t
    _ -> HailHydra . (`With` meta) <$> runMessage msg attrs
