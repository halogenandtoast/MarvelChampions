{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Treachery where

import Marvel.Prelude

import Marvel.Card
import Marvel.Id
import Marvel.Treachery.Types
import Marvel.Treachery.Treacheries

instance FromJSON Treachery where
  parseJSON = withObject "Treachery" $ \o -> do
    cardDef <- o .: "treacheryCardDef"
    withTreacheryCardCode (cdCardCode cardDef)
      $ \(_ :: TreacheryCard a) -> Treachery <$> parseJSON @a (Object o)

withTreacheryCardCode
  :: CardCode -> (forall a . IsTreachery a => TreacheryCard a -> r) -> r
withTreacheryCardCode cCode f = case lookup cCode allTreacheries of
  Nothing -> error "invalid treachery"
  Just (SomeTreacheryCard a) -> f a

allTreacheries :: HashMap CardCode SomeTreacheryCard
allTreacheries = fromList $ map
  (toFst someTreacheryCardCode)
  [ SomeTreacheryCard hardToKeepDown
  , SomeTreacheryCard imTough
  , SomeTreacheryCard stampede
  , SomeTreacheryCard explosion
  , SomeTreacheryCard falseAlarm
  , SomeTreacheryCard klawsVengeance
  , SomeTreacheryCard sonicBoom
  , SomeTreacheryCard soundManipulation
  , SomeTreacheryCard mastersOfMayhem
  , SomeTreacheryCard heartShapedHerb
  , SomeTreacheryCard ritualCombat
  , SomeTreacheryCard titaniasFury
  , SomeTreacheryCard sweepingSwoop
  , SomeTreacheryCard theVulturesPlans
  , SomeTreacheryCard electricWhipAttack
  , SomeTreacheryCard electromagneticBacklash
  , SomeTreacheryCard advance
  , SomeTreacheryCard assault
  , SomeTreacheryCard caughtOffGuard
  , SomeTreacheryCard gangUp
  , SomeTreacheryCard shadowOfThePast
  ]

lookupTreachery :: CardCode -> TreacheryId -> Treachery
lookupTreachery cardCode = case lookup cardCode allTreacheries of
  Just (SomeTreacheryCard a) -> Treachery <$> cbCardBuilder a
  Nothing -> error $ "Invalid card code for treachery " <> show cardCode
