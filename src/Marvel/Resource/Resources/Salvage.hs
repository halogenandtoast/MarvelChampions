module Marvel.Resource.Resources.Salvage (salvageSpent) where

import Marvel.Prelude

import Marvel.Card.Def
import Marvel.Card.PlayerCard.Types (PlayerCard)
import Marvel.Discard
import Marvel.Id (IdentityId)
import Marvel.Identity.Types (Field (PlayerIdentityDiscard))
import Marvel.Message.Lifted
import Marvel.Projection
import Marvel.Trait

-- | Response: After you spend this card, put a Tech upgrade from your discard pile on top
-- of your deck. Optional; not asked with none there.
-- The cards spent with it count as in the discard pile, where they are going.
salvageSpent :: (ReverseQueue m) => IdentityId -> [PlayerCard] -> m ()
salvageSpent you spent = do
  discard <- projectMap PlayerIdentityDiscard unDiscard you
  let techs = filter (cardMatch $ CardWithType UpgradeType <> CardWithTrait Tech) (discard <> filter (`notElem` discard) spent)
  unless (null techs) $
    chooseOne you $
      [targeting c (placeOnTopOfDeck you c) | c <- techs]
        <> [labeled "$label.noResponse" (pure ())]
