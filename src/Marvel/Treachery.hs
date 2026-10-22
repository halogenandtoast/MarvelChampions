{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Treachery where

import Marvel.Prelude

import Marvel.Card
import Marvel.Id
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Treachery.Treacheries
import Marvel.Treachery.Cards qualified as Cards

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
  , SomeTreacheryCard kreeManipulator
  , SomeTreacheryCard yonRoggsTreason
  , SomeTreacheryCard advance
  , SomeTreacheryCard assault
  , SomeTreacheryCard caughtOffGuard
  , SomeTreacheryCard gangUp
  , SomeTreacheryCard shadowOfThePast
  , SomeTreacheryCard androidEfficiencyA
  , SomeTreacheryCard androidEfficiencyB
  , SomeTreacheryCard androidEfficiencyC
  , SomeTreacheryCard rageOfUltron
  , SomeTreacheryCard repairSequence
  , SomeTreacheryCard swarmAttack
  , SomeTreacheryCard concussiveBlast
  , SomeTreacheryCard exhaustion
  , SomeTreacheryCard masterplan
  , SomeTreacheryCard underFire
  , SomeTreacheryCard hailHydra
  , SomeTreacheryCard harvest
  , SomeTreacheryCard deadlyShot
  , SomeTreacheryCard trickster
  , SomeTreacheryCard thoughtcasting
  , SomeTreacheryCard clashOfTheTitans
  -- The Green Goblin
  , SomeTreacheryCard intimidation
  , SomeTreacheryCard regenerativeHealing
  , SomeTreacheryCard (reprintAs Cards.gangUpAMessOfThings gangUp)
  , SomeTreacheryCard tailSweep
  , SomeTreacheryCard electromagneticPulse
  , SomeTreacheryCard lightningBolt
  , SomeTreacheryCard shockTherapy

  -- The Wrecking Crew

  -- The Rise of Red Skull
  , SomeTreacheryCard sniperShot
  , SomeTreacheryCard (reprintAs Cards.hailHydraSpiderWoman hailHydra)

  -- Zola
  , SomeTreacheryCard mindRay
  , SomeTreacheryCard technologicalEnhancements

  -- Red Skull
  , SomeTreacheryCard bitterRival
  , SomeTreacheryCard spreadingLies
  , SomeTreacheryCard infinitePower

  -- Taskmaster
  , SomeTreacheryCard mimicry
  , SomeTreacheryCard huntedByHydra

  -- The Once and Future Kang
  , SomeTreacheryCard kangsChosen
  , SomeTreacheryCard ancientGrudge

  ]

lookupTreachery :: CardCode -> TreacheryId -> Treachery
lookupTreachery cardCode = case lookup cardCode allTreacheries of
  Just (SomeTreacheryCard a) -> Treachery <$> buildCard a
  Nothing -> error $ "Invalid card code for treachery " <> show cardCode
