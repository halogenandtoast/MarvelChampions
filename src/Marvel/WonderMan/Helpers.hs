module Marvel.WonderMan.Helpers where

import Marvel.Prelude
import Marvel.Card
import Marvel.WonderMan.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Resource.Types

ionicPhysiologyFor :: HasGame m => IdentityId -> m (Maybe UpgradeId)
ionicPhysiologyFor you = selectOne $ UpgradeControlledBy (IdentityWithId you) <> UpgradeIs Cards.wm58002

ionicCards :: HasGame m => IdentityId -> m [PlayerCard]
ionicCards you = ionicPhysiologyFor you >>= maybe (pure []) heldCardsAt

tuckIonicCard :: ReverseQueue m => IdentityId -> PlayerCard -> m ()
tuckIonicCard you card = do
  host <- ionicPhysiologyFor you
  for_ host $ \uid -> do
    cards <- heldCardsAt uid
    when (length cards < 3) $ tuckPlayerCardUnder uid card

hasPrintedEnergy :: PlayerCard -> Bool
hasPrintedEnergy = elem Energy . printedResources . getCardDef
