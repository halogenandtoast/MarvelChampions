{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.SideScheme where

import Marvel.Prelude

import Marvel.Card
import Marvel.Id
import Marvel.SideScheme.SideSchemes
import Marvel.SideScheme.Types

instance FromJSON SideScheme where
  parseJSON = withObject "SideScheme" $ \o -> do
    cardDef <- o .: "sideSchemeCardDef"
    withSideSchemeCardCode (cdCardCode cardDef)
      $ \(_ :: SideSchemeCard a) -> SideScheme <$> parseJSON @a (Object o)

withSideSchemeCardCode
  :: CardCode -> (forall a . IsSideScheme a => SideSchemeCard a -> r) -> r
withSideSchemeCardCode cCode f = case lookup cCode allSideSchemes of
  Nothing -> error "invalid side scheme"
  Just (SomeSideSchemeCard a) -> f a

allSideSchemes :: HashMap CardCode SomeSideSchemeCard
allSideSchemes = fromList $ map
  (toFst someSideSchemeCardCode)
  [ SomeSideSchemeCard breakinAndTakin
  , SomeSideSchemeCard crowdControl
  , SomeSideSchemeCard bombScare
  , SomeSideSchemeCard defenseNetwork
  , SomeSideSchemeCard illegalArmsFactory
  , SomeSideSchemeCard theImmortalKlaw
  , SomeSideSchemeCard theMastersOfEvil
  , SomeSideSchemeCard usurpTheThrone
  , SomeSideSchemeCard personalChallenge
  , SomeSideSchemeCard highwayRobbery
  , SomeSideSchemeCard imminentOverload
  , SomeSideSchemeCard thePsycheMagnitron
  ]

lookupSideScheme :: CardCode -> SideSchemeId -> SideScheme
lookupSideScheme cardCode = case lookup cardCode allSideSchemes of
  Just (SomeSideSchemeCard a) -> SideScheme <$> cbCardBuilder a
  Nothing -> error $ "Invalid card code for side scheme " <> show cardCode
