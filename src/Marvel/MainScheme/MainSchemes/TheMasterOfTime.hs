module Marvel.MainScheme.MainSchemes.TheMasterOfTime where

import Marvel.Prelude

import Control.Monad (replicateM_)
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Message.Lifted
import Marvel.Modifier.Types (Modifier (RedirectAccelerationTokens))
import Marvel.Ref
import System.Random.Shuffle (shuffleM)

theMasterOfTime :: MainSchemeCard TheMasterOfTime
theMasterOfTime = mainScheme TheMasterOfTime Cards.theMasterOfTime (Static 0) (Static 0) (Static 0)

newtype TheMasterOfTime = TheMasterOfTime (Attrs MainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 2B Forced Interrupt: when an acceleration token would be placed on another scheme,
-- place it here instead.
instance IsMainScheme TheMasterOfTime where
  mainSchemeModifiersFor _ (MainSchemeRef other) (TheMasterOfTime attrs)
    | other /= mainSchemeId attrs = pure [RedirectAccelerationTokens (mainSchemeId attrs)]
  mainSchemeModifiersFor _ _ _ = pure []

-- | The stage 3 schemes, one revealed at random by each player.
stageThrees :: [CardDef]
stageThrees = [Cards.theChronopolis, Cards.inexorableFate, Cards.theRealmOfRamaTut, Cards.thePresentFutureWar]

-- | 2A When Revealed: place 1 acceleration token here for each side scheme in play, then
-- discard each side scheme. Each player reveals a random stage 3A in turn order, each in
-- a game area of their own; the unused stage 3 schemes are removed from the game.
--
-- 2B, in no other area, has no threat and is never completed. Players cannot join
-- this area while there are others; when all have, it advances to stage 4A (the
-- scenario's "kangJoinAnotherArea").
instance RunMessage TheMasterOfTime where
  runMessage msg ms@(TheMasterOfTime attrs) = runQueueT $ case msg of
    MainSchemeMessage ident msg' | ident == mainSchemeId attrs -> case msg' of
      RevealMainScheme -> do
        sideSchemes <- concatMapM (getSideSchemesIn . areaId) =<< getAreas
        replicateM_ (length sideSchemes) (placeAccelerationToken (mainSchemeId attrs))
        for_ sideSchemes (removeFromPlay . SideSchemeRef)
        active <- getActivePlayerId
        players <- getAllPlayers
        stages <- shuffleM stageThrees
        for_ (zip players stages) \(p, stage) -> do
          setActivePlayer p
          _ <- createArea [p]
          addMainScheme (toCardCode stage)
        setActivePlayer active
        pure ms
      MainSchemePlaceThreat _ -> pure ms
      _ -> TheMasterOfTime <$> runMessage msg attrs
    _ -> TheMasterOfTime <$> runMessage msg attrs
