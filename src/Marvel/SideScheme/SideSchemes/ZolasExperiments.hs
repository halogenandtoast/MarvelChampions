module Marvel.SideScheme.SideSchemes.ZolasExperiments (
  zolasExperiments,
  ZolasExperiments (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait
import Marvel.Window.Types qualified as W

zolasExperiments :: SideSchemeCard ZolasExperiments
zolasExperiments = sideScheme ZolasExperiments Cards.zolasExperiments (PerPlayer 3)

newtype ZolasExperiments = ZolasExperiments (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Forced Response: after a minion enters play, the topmost Tech attachment in the
-- encounter discard pile is attached to it.
instance RunMessage ZolasExperiments where
  runMessage msg s@(ZolasExperiments attrs) = runQueueT $ case msg of
    CheckWindows ws | mid : _ <- [m | W.Window W.After (W.MinionEnteredPlay m) <- ws] -> do
      Pile _ discard <- getPile EncounterDeckKey
      for_ (headMay $ filter (cardMatch $ CardWithType AttachmentType <> CardWithTrait Tech) discard) \card ->
        attachEncounterCard card mid
      pure s
    _ -> ZolasExperiments <$> runMessage msg attrs
