{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Ally
  ( module Marvel.Ally
  , module X
  ) where

import Marvel.Prelude

import Marvel.Ally.Allies
import Marvel.Ally.Types
import Marvel.Ally.Types as X (Ally)
import Marvel.Id

instance FromJSON Ally where
  parseJSON = withObject "Ally" $ \o -> do
    cardDef <- o .: "allyCardDef"
    withAllyCardCode (cdCardCode cardDef)
      $ \(_ :: AllyCard a) -> Ally <$> parseJSON @a (Object o)

withAllyCardCode :: CardCode -> (forall a . IsAlly a => AllyCard a -> r) -> r
withAllyCardCode cCode f = case lookup cCode allAllies of
  Nothing -> error "invalid ally"
  Just (SomeAllyCard a) -> f a

lookupAlly :: CardCode -> IdentityId -> AllyId -> Ally
lookupAlly cardCode identityId allyId = case lookup cardCode allAllies of
  Just (SomeAllyCard a) -> Ally $ cbCardBuilder a (identityId, allyId)
  Nothing -> error $ "Invalid card code for ally " <> show cardCode

allAllies :: HashMap CardCode SomeAllyCard
allAllies = fromList $ map
  (toFst someAllyCardCode)
  [ SomeAllyCard blackCatFeliciaHardy
  , SomeAllyCard spiderWomanJessicaDrew
  , SomeAllyCard hellcatPatsyWalker
  , SomeAllyCard warMachineJamesRhodes
  , SomeAllyCard shuri
  , SomeAllyCard hulkBruceBanner
  , SomeAllyCard tigraGreerGrantNelson
  , SomeAllyCard daredevilMattMurdock
  , SomeAllyCard jessicaJones
  , SomeAllyCard hawkeyeClintBarton
  , SomeAllyCard mariaHill
  , SomeAllyCard vision
  , SomeAllyCard blackWidowNatashaRomanoff
  , SomeAllyCard lukeCage
  , SomeAllyCard mockingbirdBobbiMorse
  , SomeAllyCard nickFury
  ]
