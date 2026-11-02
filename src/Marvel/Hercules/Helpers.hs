module Marvel.Hercules.Helpers where

import Marvel.Prelude
import Data.Aeson.KeyMap qualified as KM
import Marvel.Identity.Types
import Marvel.Projection
import Marvel.Game.Source
import Marvel.Card
import Marvel.Id
import Marvel.Matchers
import Marvel.Ref
import Marvel.Query
import Marvel.Trait qualified as T

-- Appeal to Athena suppresses counting rather than stripping Gift traits.
giftCount :: HasGame m => IdentityId -> m Natural
giftCount you = do
  obligations <- getObligationsOf you
  blocked <- or <$> traverse (\oid -> getsGame $ maybe False ((== "59035") . toCardCode) . (`abilityCardDef` ObligationRef oid)) obligations
  if blocked then pure 0 else fromIntegral . length <$> selectList (UpgradeControlledBy (IdentityWithId you) <> UpgradeWithTrait T.Gift)

herculesLaborAvailable :: HasGame m => IdentityId -> m Bool
herculesLaborAvailable you = do
  sides <- project PlayerIdentitySides you
  let hasCards = any (\case
        AlterEgoSide ae -> case toJSON ae of
          Object o -> case KM.lookup "laborDeck" o of
            Just value -> case fromJSON value of Success (cards :: [EncounterCard]) -> notNull cards; _ -> False
            _ -> False
          _ -> False
        _ -> False) sides
  attachments <- selectList $ AttachmentWithTrait T.Labor
  obligations <- getObligationsOf you
  activeObligation <- or <$> traverse (\oid -> getsGame $ maybe False (member T.Labor . cdTraits) . (`abilityCardDef` ObligationRef oid)) obligations
  pure $ hasCards && null attachments && not activeObligation
