module Marvel.SideScheme.SideSchemes.LightOfCenturiesSphere (
  lightOfCenturiesSphere,
  LightOfCenturiesSphere (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

lightOfCenturiesSphere :: SideSchemeCard LightOfCenturiesSphere
lightOfCenturiesSphere =
  sideScheme (LightOfCenturiesSphere . (`With` Meta Nothing)) Cards.lightOfCenturiesSphere (PerPlayer 3)

-- | The player whose thwart defeats it.
newtype Meta = Meta {defeatedBy :: Maybe IdentityId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype LightOfCenturiesSphere = LightOfCenturiesSphere (Attrs SideScheme `With` Meta)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance HasCardCode LightOfCenturiesSphere where
  toCardCode (LightOfCenturiesSphere (a `With` _)) = toCardCode a

instance IsSideScheme LightOfCenturiesSphere where
  toSideSchemeAttrs (LightOfCenturiesSphere (a `With` _)) = a

-- | When Defeated: discard from the encounter deck until a minion is discarded, and put
-- it into play engaged with the player who defeated this scheme.
instance RunMessage LightOfCenturiesSphere where
  runMessage msg s@(LightOfCenturiesSphere (attrs `With` meta)) = runQueueT $ case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      SideSchemeThwarted source _ -> do
        player <- maybe getActivePlayerId pure =<< getSourceController source
        attrs' <- runMessage msg attrs
        pure . LightOfCenturiesSphere $ attrs' `With` Meta (Just player)
      DefeatSideScheme -> do
        discardUntil FromEncounterDeck (CardWithType MinionType) attrs
        pure s
      _ -> LightOfCenturiesSphere . (`With` meta) <$> runMessage msg attrs
    WithDiscardedMatch target FromEncounterDeck (EncounterCard minion) | isTarget attrs target -> do
      player <- maybe getActivePlayerId pure (defeatedBy meta)
      putEncounterCardIntoPlay player minion
      pure s
    _ -> LightOfCenturiesSphere . (`With` meta) <$> runMessage msg attrs
