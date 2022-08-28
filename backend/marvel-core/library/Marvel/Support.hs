{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Support where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Id
import Marvel.Support.Supports
import Marvel.Support.Types

instance FromJSON Support where
  parseJSON = withObject "Support" $ \o -> do
    cardDef <- o .: "supportCardDef"
    withSupportCardCode (cdCardCode cardDef)
      $ \(_ :: SupportCard a) -> Support <$> parseJSON @a (Object o)

withSupportCardCode
  :: CardCode -> (forall a . IsSupport a => SupportCard a -> r) -> r
withSupportCardCode cCode f = case lookup cCode allSupports of
  Nothing -> error "invalid support"
  Just (SomeSupportCard a) -> f a

allSupports :: HashMap CardCode SomeSupportCard
allSupports = fromList $ map
  (toFst someSupportCardCode)
  [ SomeSupportCard auntMay
  , SomeSupportCard alphaFlightStation
  , SomeSupportCard superhumanLawDivision
  , SomeSupportCard pepperPotts
  , SomeSupportCard starkTower
  , SomeSupportCard theGoldenCity
  , SomeSupportCard tacTeam
  , SomeSupportCard interrogationRoom
  , SomeSupportCard surveillanceTeam
  , SomeSupportCard theTriskellion
  , SomeSupportCard medTeam
  , SomeSupportCard avengersMansion
  , SomeSupportCard helicarrier
  ]

lookupSupport :: CardCode -> IdentityId -> SupportId -> Support
lookupSupport cardCode identityId supportId =
  case lookup cardCode allSupports of
    Just (SomeSupportCard a) ->
      Support $ cbCardBuilder a (identityId, supportId)
    Nothing -> error $ "Invalid card code for support " <> show cardCode
