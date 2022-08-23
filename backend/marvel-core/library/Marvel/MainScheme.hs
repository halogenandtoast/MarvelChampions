{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.MainScheme where

import Marvel.Prelude

import Marvel.Card
import Marvel.Id
import Marvel.MainScheme.Types
import Marvel.MainScheme.MainSchemes

instance FromJSON MainScheme where
  parseJSON v = flip (withObject "MainScheme") v $ \o -> do
    cardDef <- o .: "mainSchemeCardDef"
    withMainSchemeCardCode (cdCardCode cardDef)
      $ \(_ :: MainSchemeCard a) -> MainScheme <$> parseJSON @a v

withMainSchemeCardCode
  :: CardCode -> (forall a . IsMainScheme a => MainSchemeCard a -> r) -> r
withMainSchemeCardCode cCode f = case lookup cCode allMainSchemes of
  Nothing -> error "invalid minion"
  Just (SomeMainSchemeCard a) -> f a

allMainSchemes :: HashMap CardCode SomeMainSchemeCard
allMainSchemes = fromList $ map
  (toFst someMainSchemeCardCode)
  [ SomeMainSchemeCard theBreakIn
  , SomeMainSchemeCard undergroundDistribution
  , SomeMainSchemeCard secretRendezvous
      -- , SomeMainSchemeCard theCrimsonCowl
      -- , SomeMainSchemeCard assaultOnNorad
      -- , SomeMainSchemeCard countdownToOblivion
  ]

lookupMainScheme :: CardCode -> MainSchemeId -> Maybe MainScheme
lookupMainScheme cardCode mainSchemeId = case lookup cardCode allMainSchemes of
  Just (SomeMainSchemeCard a) ->
    Just . MainScheme $ cbCardBuilder a mainSchemeId
  Nothing -> Nothing
