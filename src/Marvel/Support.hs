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
  $ concatMap (\(SomeSupportCard a) -> SomeSupportCard <$> printings a)
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
  , SomeSupportCard stevesApartment
  , SomeSupportCard quinjet
  , SomeSupportCard avengersTower
  , SomeSupportCard aamirKhan
  , SomeSupportCard brunoCarrelli
  , SomeSupportCard nakiaBahadir
  , SomeSupportCard safeHouse29
  , SomeSupportCard quincarrier
  , SomeSupportCard asgard
  , SomeSupportCard hallOfHeroes
  , SomeSupportCard sanctumSanctorum
  , SomeSupportCard theNightNurse
  , SomeSupportCard bannersLaboratory
  , SomeSupportCard beatCop
  -- The Green Goblin

  -- The Wrecking Crew

  -- The Rise of Red Skull

  -- The Once and Future Kang

  ]

lookupSupport :: CardCode -> IdentityId -> SupportId -> Support
lookupSupport cardCode identityId supportId =
  case lookup cardCode allSupports of
    Just (SomeSupportCard a) ->
      Support $ buildCard a (identityId, supportId)
    Nothing -> error $ "Invalid card code for support " <> show cardCode
