module Marvel.MainScheme.MainSchemes.TheHood where

import Control.Monad (replicateM_)
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Message.Lifted
import Marvel.Prelude
import Marvel.Ref
import Marvel.Villain.Villains.TheHood

newtype HoodScheme = HoodScheme (Attrs MainScheme `With` HoodSchemeMeta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype HoodSchemeMeta = HoodSchemeMeta {hoodMisses :: Natural}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsMainScheme HoodScheme where
  toMainSchemeAttrs (HoodScheme (a `With` _)) = a

instance HasCardCode HoodScheme where
  toCardCode (HoodScheme (a `With` _)) = toCardCode a

hoodScheme :: Attrs MainScheme -> HoodScheme
hoodScheme a = HoodScheme (a `With` HoodSchemeMeta 0)

makingConnections, promisedProsperity, crimeState :: MainSchemeCard HoodScheme
makingConnections = mainScheme hoodScheme Cards.makingConnections (PerPlayer 5) (PerPlayer 1) (PerPlayer 1)
promisedProsperity = mainScheme hoodScheme Cards.promisedProsperity (PerPlayer 8) (PerPlayer 2) (PerPlayer 1)
crimeState = mainScheme hoodScheme Cards.crimeState (PerPlayer 10) (PerPlayer 3) (PerPlayer 1)

instance RunMessage HoodScheme where
  runMessage msg e@(HoodScheme (attrs `With` meta)) = runQueueT $ case msg of
    SetupMainScheme | toCardCode attrs == "24004" -> e <$ recruitModular
    MainSchemeMessage mid RevealMainScheme | mid == mainSchemeId attrs -> do
      when (toCardCode attrs /= "24004") do
        recruitModular
        placeAccelerationToken mid
      players <- getPlayers
      for_ players \you -> cardSpecific GameRef "hoodFoulPlay" (you, if toCardCode attrs == "24005" then Just mid else Nothing)
      when (toCardCode attrs == "24005") $ cardSpecific attrs "hoodFoulPlayPenalty" ()
      pure $ hoodScheme attrs
    CardSpecific (isTarget attrs -> True) "hoodFoulPlayMiss" _ ->
      pure $ HoodScheme (attrs `With` HoodSchemeMeta (hoodMisses meta + 1))
    CardSpecific (isTarget attrs -> True) "hoodFoulPlayPenalty" _ -> do
      replicateM_ (fromIntegral $ hoodMisses meta) (placeThreat (mainSchemeId attrs) 2)
      pure $ hoodScheme attrs
    AccelerateMainScheme | toCardCode attrs == "24006" -> do
      attrs' <- runMessage msg attrs
      forcedResponse (mainSchemeId attrs) $ traverse_ foulPlay =<< getPlayers
      pure $ HoodScheme (attrs' `With` meta)
    AdvanceMainScheme mid | mid == mainSchemeId attrs -> do
      if toCardCode attrs == "24006" then gameOver Lost else nextMainScheme mid
      pure e
    _ -> (\a -> HoodScheme (a `With` meta)) <$> runMessage msg attrs
