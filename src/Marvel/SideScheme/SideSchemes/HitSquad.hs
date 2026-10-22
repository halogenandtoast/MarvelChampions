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
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
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
  runMessage msg s@(HitSquad attrs) = case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      push $ cardSpecific attrs "discard" players
      pure s
    CardSpecific (isTarget attrs -> True) "discard" (fromJSON -> Success (players :: [IdentityId])) ->
      case players of
        [] -> pure s
        p : rest ->
          getEncounterDeck >>= \case
            -- the deck is made again from the discard pile first
            [] -> s <$ pushAll [EmptyScenarioDeck, cardSpecific attrs "discard" players]
            card : _ -> do
              let n = boostCount $ cdBoostIcons $ getCardDef card
              pushAll $
                DiscardTopOfEncounterDeck 1 Nothing
                  : [msg' | n > 0, msg' <- damageIdentity p (toSource attrs) (toDamage n FromTreachery)]
                    <> [cardSpecific attrs "discard" rest]
              pure s
    _ -> HitSquad <$> runMessage msg attrs
