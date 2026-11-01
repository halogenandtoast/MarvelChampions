module Marvel.NextEvolution.Stryfe where

import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source
import Marvel.Id
import Marvel.Hand
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Projection
import Marvel.Query
import Marvel.Ref ()

handTypeCount :: HasGame m => IdentityId -> m Natural
handTypeCount you = do
  cards <- unHand <$> project PlayerIdentityHand you
  pure $ fromIntegral $ maximum (0 : [length [c | c <- cards, cdCardType (getCardDef c) == t] | t <- [AllyType, EventType, ResourceType, SupportType, UpgradeType, PlayerSideSchemeType]])

returnControlledCard :: (ReverseQueue m) => IdentityId -> m ()
returnControlledCard you = do
  upgrades <- selectList (upgradeControlledBy you)
  supports <- selectList (SupportControlledBy $ IdentityWithId you)
  unless (null upgrades && null supports) $ chooseOne you $
    [targeting u (returnToHand u) | u <- upgrades] <> [targeting s (returnToHand s) | s <- supports]
