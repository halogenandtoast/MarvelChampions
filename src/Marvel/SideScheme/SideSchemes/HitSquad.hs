module Marvel.SideScheme.SideSchemes.HitSquad (
  hitSquad,
  HitSquad (..),
) where

import Marvel.Prelude

import Data.Aeson ()
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
              discardTopOfEncounterDeck 1
              countBoostIcons attrs p [card]
              cardSpecific attrs "discard" rest
              pure s
    WithBoostIcons (isTarget attrs -> True) (fromJSON -> Success p) (sum . map snd -> n) -> do
      when (n > 0) $ damageIdentity p attrs (toDamage n FromTreachery)
      pure s
    _ -> HitSquad <$> runMessage msg attrs
