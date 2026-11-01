module Marvel.Nightcrawler.Helpers where

import Marvel.Prelude
import Data.HashMap.Strict qualified as HashMap
import Marvel.Card.Side (Side)
import Marvel.Identity.Types qualified as Identity
import Marvel.Modifier.Types (Modifier (CannotChangeForm, CannotChangeToHeroForm))
import Marvel.Projection (project)
import Marvel.Game.Source
import Marvel.Id

-- Defined base powers, including an X whose value is defined by a card.
basePowers :: HasGame m => CharacterId -> m (Natural, Natural)
basePowers = getCharacterBasePowers

currentThwart :: HasGame m => CharacterId -> m Natural
currentThwart = getCharacterThwart

-- An imposed form lock can leave no other legal face. Callers must still resolve
-- their independent effects instead of asking an empty form question.
legalOtherForms :: HasGame m => IdentityId -> m [Side]
legalOtherForms you = do
  current <- project Identity.PlayerIdentitySide you
  sides <- project Identity.PlayerIdentitySides you
  modifiers <- getModifiers you
  pure [side | (side, face) <- HashMap.toList sides, side /= current, CannotChangeForm `notElem` modifiers,
    case face of Identity.HeroSide _ -> CannotChangeToHeroForm `notElem` modifiers; _ -> True]
