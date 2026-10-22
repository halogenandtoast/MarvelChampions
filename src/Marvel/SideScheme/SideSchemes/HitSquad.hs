module Marvel.SideScheme.SideSchemes.HitSquad (
  hitSquad,
  HitSquad (..),
) where

import Marvel.Prelude

import Data.Aeson (Result (..), fromJSON)
import Marvel.Boost (boostCount)
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

hitSquad :: SideSchemeCard HitSquad
hitSquad = sideScheme HitSquad Cards.hitSquad (PerPlayer 3)

newtype HitSquad = HitSquad (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: in player order, each player discards the top card of the encounter
-- deck and takes 1 damage for each boost icon on it.
instance RunMessage HitSquad where
  runMessage msg s@(HitSquad attrs) = runQueueT $ case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      cardSpecific attrs "discard" players
      pure s
    CardSpecific (isTarget attrs -> True) "discard" (fromJSON -> Success (players :: [IdentityId])) ->
      case players of
        [] -> pure s
        p : rest ->
          getEncounterDeck >>= \case
            [] -> s <$ cardSpecific attrs "discard" rest
            card : _ -> do
              let n = boostCount $ cdBoostIcons $ getCardDef card
              discardTopOfEncounterDeck 1
              when (n > 0) $ damageHero p attrs (toDamage n FromTreachery)
              cardSpecific attrs "discard" rest
              pure s
    _ -> HitSquad <$> runMessage msg attrs
