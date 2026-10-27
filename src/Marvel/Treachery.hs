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
  , SomeTreacheryCard allInADaysWork
  , SomeTreacheryCard deathFromAbove
  , SomeTreacheryCard iSeeYou
  , SomeTreacheryCard madGenius
  , SomeTreacheryCard overconfidence
  , SomeTreacheryCard wickedAmbitions
  , SomeTreacheryCard intimidation
  , SomeTreacheryCard regenerativeHealing
  , SomeTreacheryCard (reprintAs Cards.gangUpAMessOfThings gangUp)
  , SomeTreacheryCard tailSweep
  , SomeTreacheryCard electromagneticPulse
  , SomeTreacheryCard lightningBolt
  , SomeTreacheryCard shockTherapy

  -- The Wrecking Crew
  , SomeTreacheryCard buddySystem
  , SomeTreacheryCard (reprintAs Cards.buddySystemThunderball buddySystem)
  , SomeTreacheryCard (reprintAs Cards.buddySystemPiledriver buddySystem)
  , SomeTreacheryCard (reprintAs Cards.buddySystemBulldozer buddySystem)
  , SomeTreacheryCard chaosInThePrison
  , SomeTreacheryCard (reprintAs Cards.chaosInThePrisonThunderball chaosInThePrison)
  , SomeTreacheryCard (reprintAs Cards.chaosInThePrisonBulldozer chaosInThePrison)
  , SomeTreacheryCard crowbarToss
  , SomeTreacheryCard getWrecked
  , SomeTreacheryCard (reprintAs Cards.getWreckedThunderball getWrecked)
  , SomeTreacheryCard (reprintAs Cards.getWreckedPiledriver getWrecked)
  , SomeTreacheryCard (reprintAs Cards.getWreckedBulldozer getWrecked)
  , SomeTreacheryCard iveBeenWaitingForThis
  , SomeTreacheryCard (reprintAs Cards.iveBeenWaitingForThisThunderball iveBeenWaitingForThis)
  , SomeTreacheryCard (reprintAs Cards.iveBeenWaitingForThisPiledriver iveBeenWaitingForThis)
  , SomeTreacheryCard mysticalLink
  , SomeTreacheryCard youreDeadMeat
  , SomeTreacheryCard energyProjectiles
  , SomeTreacheryCard lightningBlast
  , SomeTreacheryCard tacticalProwess
  , SomeTreacheryCard oversizedHands
  , SomeTreacheryCard escapePlan
  , SomeTreacheryCard pummel
  , SomeTreacheryCard uncannyResilience
  , SomeTreacheryCard bullRush
  , SomeTreacheryCard headbutt
  , SomeTreacheryCard leadingTheCharge

  -- The Rise of Red Skull
  , SomeTreacheryCard sniperShot
  , SomeTreacheryCard (reprintAs Cards.hailHydraSpiderWoman hailHydra)
  , SomeTreacheryCard fullAuto
  , SomeTreacheryCard hardAsNails
  , SomeTreacheryCard raidTheArmory
  , SomeTreacheryCard stallTactics
  , SomeTreacheryCard swingingStone
  , SomeTreacheryCard steelKick
  , SomeTreacheryCard piercingThorns
  , SomeTreacheryCard omniMorphDuplication
  , SomeTreacheryCard icyGrip
  , SomeTreacheryCard (reprintAs Cards.hailHydraHydraAssault hailHydra)
  , SomeTreacheryCard weaponMaster
  , SomeTreacheryCard concussionGrenade

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
  , SomeTreacheryCard energyBlast
  , SomeTreacheryCard manipulatedTimestream
  , SomeTreacheryCard timeTravelTactics
  , SomeTreacheryCard pastMachinations


  -- Ant-Man
  , SomeTreacheryCard yellowjacketsPlan

  -- Wasp
  , SomeTreacheryCard beetleMania

  -- Quicksilver
  , SomeTreacheryCard earthquake

  -- Scarlet Witch
  , SomeTreacheryCard chaosManipulation

  -- Star-Lord
  , SomeTreacheryCard spartoiCunning

  -- Gamora
  , SomeTreacheryCard waylay

  -- Rocket Raccoon
  , SomeTreacheryCard planetaryInvasion
  ]

lookupTreachery :: CardCode -> TreacheryId -> Treachery
lookupTreachery cardCode = case lookup cardCode allTreacheries of
  Just (SomeTreacheryCard a) -> Treachery <$> buildCard a
  Nothing -> error $ "Invalid card code for treachery " <> show cardCode
