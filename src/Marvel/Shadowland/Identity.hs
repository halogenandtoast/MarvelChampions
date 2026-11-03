module Marvel.Shadowland.Identity (heroWithTitleInGame) where

import Marvel.Prelude
import Data.HashMap.Strict qualified as HashMap
import Marvel.Card
import Marvel.Game.Source
import Marvel.Identity.Types (Field(PlayerIdentitySides),PlayerIdentitySide(HeroSide))
import Marvel.Projection
import Marvel.Name (title)

-- Campaign identity checks include the hero even during alter-ego setup.
heroWithTitleInGame :: HasGame m => Text -> m Bool
heroWithTitleInGame name = do
  players <- getAllPlayers
  sides <- traverse (project PlayerIdentitySides) players
  let names = [title (cdName $ getCardDef hero) | HeroSide hero <- concatMap HashMap.elems sides]
  pure $ name `elem` names
