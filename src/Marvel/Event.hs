{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Event where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Event.Events
import Marvel.Event.Types
import Marvel.Id

instance FromJSON Event where
  parseJSON = withObject "Event" $ \o -> do
    cardDef <- o .: "eventCardDef"
    withEventCardCode (cdCardCode cardDef)
      $ \(_ :: EventCard a) -> Event <$> parseJSON @a (Object o)

withEventCardCode
  :: CardCode -> (forall a . IsEvent a => EventCard a -> r) -> r
withEventCardCode cCode f = case lookup cCode allEvents of
  Nothing -> error "invalid event"
  Just (SomeEventCard a) -> f a

allEvents :: HashMap CardCode SomeEventCard
allEvents = fromList $ map
  (toFst someEventCardCode)
  $ concatMap (\(SomeEventCard a) -> SomeEventCard <$> printings a)
  [ SomeEventCard fw53003, SomeEventCard fw53004, SomeEventCard fw53005, SomeEventCard fw53019, SomeEventCard fw54004, SomeEventCard fw54005, SomeEventCard fw54006, SomeEventCard fw54014, SomeEventCard fw54016, SomeEventCard fw54022, SomeEventCard fw54023
  , SomeEventCard aosP50003, SomeEventCard aosP50004, SomeEventCard aosP50005, SomeEventCard aosP50006, SomeEventCard aosP50024, SomeEventCard aosP50037, SomeEventCard aosP50038, SomeEventCard aosP50039
  , SomeEventCard clawedStrikeShuri, SomeEventCard ontheProwlShuri, SomeEventCard wakandaForeverShuri, SomeEventCard infiltrationShuri, SomeEventCard heartofthePantherShuri, SomeEventCard smoothasSilkSilk, SomeEventCard swingingSilkKickSilk, SomeEventCard wallcrawlSilk, SomeEventCard stopHittingYourselfSilk, SomeEventCard investigativeJournalismSilk, SomeEventCard quickQuipSilk
  , SomeEventCard nc48007, SomeEventCard nc48008, SomeEventCard nc48009, SomeEventCard nc48010, SomeEventCard nc48011, SomeEventCard nc48016, SomeEventCard nc48018, SomeEventCard nc48031, SomeEventCard nc48032
  , SomeEventCard jubilee47006, SomeEventCard jubilee47007a, SomeEventCard jubilee47007b, SomeEventCard jubilee47007c, SomeEventCard jubilee47008a, SomeEventCard jubilee47008b, SomeEventCard jubilee47008c, SomeEventCard jubilee47009, SomeEventCard jubilee47014, SomeEventCard jubilee47015, SomeEventCard jubilee47021, SomeEventCard jubilee47022, SomeEventCard jubilee47028, SomeEventCard jubilee47029
  , SomeEventCard takeThat
  , SomeEventCard surpriseMove
  , SomeEventCard chillOut
  , SomeEventCard iceBlast
  , SomeEventCard arcticAttack
  , SomeEventCard electromagneticBlast, SomeEventCard metalShards, SomeEventCard magneticMissile, SomeEventCard squaredOff, SomeEventCard nobleSacrifice, SomeEventCard youGotThis, SomeEventCard faceThePast
  , SomeEventCard basicSpell
  , SomeEventCard fullBodyCharge
  , SomeEventCard magicBarrier
  , SomeEventCard soulStrike
  , SomeEventCard exorcism
  , SomeEventCard steppingDisc
  , SomeEventCard scrying
  , SomeEventCard concussiveBlast, SomeEventCard commandAuthority, SomeEventCard energyConversion, SomeEventCard sideBySide, SomeEventCard suitUp
  , SomeEventCard exhaustingPersonality, SomeEventCard maximumEffort, SomeEventCard metaknowledge, SomeEventCard yooHoo, SomeEventCard thisCardIsFire, SomeEventCard barelyAScratch, SomeEventCard cutupper, SomeEventCard daBomb, SomeEventCard getRageY, SomeEventCard iGotThis, SomeEventCard notMyResponsibility, SomeEventCard poolInspection, SomeEventCard breakTime, SomeEventCard getInFrontOfMe, SomeEventCard mulligan
  , SomeEventCard animalInstinct, SomeEventCard clawMastery, SomeEventCard regenerativeLongevity, SomeEventCard sisterlyBond, SomeEventCard criticalHit, SomeEventCard predictablePloy, SomeEventCard anticipatedAttack
  , SomeEventCard adaptivePlumage, SomeEventCard aerialAgility, SomeEventCard metamorphosis, SomeEventCard naturalFlight, SomeEventCard razorDive, SomeEventCard aerialIntervention, SomeEventCard taunt, SomeEventCard bombsAway, SomeEventCard flyingFormation
  , SomeEventCard flurryOfBlades, SomeEventCard mentalDetection, SomeEventCard psionicRedirect, SomeEventCard telepathicSuggestion, SomeEventCard upsideTheHead, SomeEventCard directedForce, SomeEventCard soaringHearts, SomeEventCard psiBowAttack, SomeEventCard psiFlailStrike
  , SomeEventCard thePosse
  , SomeEventCard teamInvestigation
  , SomeEventCard rightPlaceRightTime
  , SomeEventCard luckBeaLady
  , SomeEventCard aGoodWorkout
  , SomeEventCard cable40002, SomeEventCard cable40003, SomeEventCard cable40004, SomeEventCard cable40005, SomeEventCard cable40017, SomeEventCard cable40026
  , SomeEventCard goinRogue, SomeEventCard southernCross, SomeEventCard energyTransfer, SomeEventCard bulletproofBelle, SomeEventCard superpowerAdaptation, SomeEventCard notToday, SomeEventCard chargedCard, SomeEventCard royalFlush, SomeEventCard naturalAgility, SomeEventCard creoleCharmer, SomeEventCard breakingAndEntering, SomeEventCard beautyAndTheThief, SomeEventCard mutantEducation, SomeEventCard xMenInstruction
  , SomeEventCard weatherGoddess
  , SomeEventCard torrentialRain
  , SomeEventCard lightningBolt
  , SomeEventCard flashFreeze
  , SomeEventCard blastOfWind
  , SomeEventCard toMeMyXMen
  , SomeEventCard fullBlast, SomeEventCard ricochetBeam, SomeEventCard tacticalBrilliance, SomeEventCard gameTime
  , SomeEventCard berserkerBarrage, SomeEventCard sliceAndDice, SomeEventCard lungingStrike, SomeEventCard trackByScent, SomeEventCard regenerativeHealing, SomeEventCard outtaMyWay, SomeEventCard precisionStrike, SomeEventCard fastballSpecial
  , SomeEventCard backflip
  , SomeEventCard enhancedSpiderSense
  , SomeEventCard swingingWebKick
  , SomeEventCard crisisInterdiction
  , SomeEventCard photonicBlast
  , SomeEventCard gammaSlam
  , SomeEventCard groundStomp
  , SomeEventCard legalPractice
  , SomeEventCard oneTwoPunch
  , SomeEventCard splitPersonality
  , SomeEventCard repulsorBlast
  , SomeEventCard supersonicPunch
  , SomeEventCard ancestralKnowledge
  , SomeEventCard wakandaForeverA
  , SomeEventCard wakandaForeverB
  , SomeEventCard wakandaForeverC
  , SomeEventCard wakandaForeverD
  , SomeEventCard chaseThemDown
  , SomeEventCard relentlessAssault
  , SomeEventCard uppercut
  , SomeEventCard forJustice
  , SomeEventCard greatResponsibility
  , SomeEventCard getReady
  , SomeEventCard leadFromTheFront
  , SomeEventCard makeTheCall
  , SomeEventCard counterPunch
  , SomeEventCard getBehindMe
  , SomeEventCard emergency
  , SomeEventCard firstAid
  , SomeEventCard haymaker
  , SomeEventCard fearlessDetermination
  , SomeEventCard heroicStrike
  , SomeEventCard shieldBlock
  , SomeEventCard shieldToss
  , SomeEventCard avengersAssemble
  , SomeEventCard strengthInNumbers
  , SomeEventCard expertDefense
  , SomeEventCard bigHands
  , SomeEventCard sneakBy
  , SomeEventCard wiggleRoom
  , SomeEventCard preemptiveStrike
  , SomeEventCard tackle
  , SomeEventCard melee
  , SomeEventCard concussiveBlow
  , SomeEventCard moraleBoost
  , SomeEventCard covertOps
  , SomeEventCard danceOfDeath
  , SomeEventCard stealthStrike
  , SomeEventCard defenderOfTheNineRealms
  , SomeEventCard forAsgard
  , SomeEventCard hammerThrow
  , SomeEventCard lightningStrike
  , SomeEventCard getOverHere
  , SomeEventCard meanSwing
  , SomeEventCard invulnerability
  , SomeEventCard teamwork
  , SomeEventCard secondWind
  , SomeEventCard masterOfTheMysticArts
  , SomeEventCard crimsonBandsOfCyttorak
  , SomeEventCard imagesOfIkonn
  , SomeEventCard sevenRingsOfRaggadorr
  , SomeEventCard vaporsOfValtorr
  , SomeEventCard windsOfWatoomb
  , SomeEventCard astralProjection
  , SomeEventCard magicBlast
  , SomeEventCard mysticalStudies
  , SomeEventCard protectiveWard
  , SomeEventCard desperateDefense
  , SomeEventCard momentumShift
  , SomeEventCard warning
  , SomeEventCard skilledStrike
  , SomeEventCard foiled
  , SomeEventCard crushingBlow
  , SomeEventCard hulkSmash
  , SomeEventCard subOrbitalLeap
  , SomeEventCard thunderclap
  , SomeEventCard unstoppableForce
  , SomeEventCard dropKick
  , SomeEventCard toeToToe
  , SomeEventCard youllPayForThat
  , SomeEventCard toTheRescue
  , SomeEventCard inspiringPresence
  -- The Green Goblin

  -- The Wrecking Crew

  -- The Rise of Red Skull
  , SomeEventCard sonicArrow
  , SomeEventCard explosiveArrow
  , SomeEventCard electricArrow
  , SomeEventCard cableArrow
  , SomeEventCard vibraniumArrow
  , SomeEventCard readyForAction
  , SomeEventCard earthsMightiestHeroes
  , SomeEventCard venomBlast
  , SomeEventCard pheromones
  , SomeEventCard contaminantImmunity
  , SomeEventCard inconspicuous
  , SomeEventCard selfPropelledGlide
  , SomeEventCard pressTheAdvantage
  , SomeEventCard piercingStrike
  , SomeEventCard clearTheArea

  -- The Once and Future Kang


  -- Ant-Man
  , SomeEventCard giantStomp
  , SomeEventCard hiveMind
  , SomeEventCard resize
  , SomeEventCard callForAid
  , SomeEventCard moxie
  , SomeEventCard swarmTactics
  , SomeEventCard momentOfTriumph
  , SomeEventCard layDownTheLaw
  , SomeEventCard musterCourage
  , SomeEventCard assessTheSituation

  -- Wasp
  , SomeEventCard giantHelp
  , SomeEventCard pinpointStrike
  , SomeEventCard rapidGrowth
  , SomeEventCard waspSting
  , SomeEventCard intoTheFray
  , SomeEventCard surpriseAttack
  , SomeEventCard athleticConditioning
  , SomeEventCard runningInterference
  , SomeEventCard allForOne
  , SomeEventCard perseverance

  -- Quicksilver
  , SomeEventCard alwaysBeRunning
  , SomeEventCard doubleTime
  , SomeEventCard maximumVelocity
  , SomeEventCard speedCyclone
  , SomeEventCard neverBackDown
  , SomeEventCard sideStep
  , SomeEventCard orderAndChaos
  , SomeEventCard unitedWeStand
  , SomeEventCard beatEmUp

  -- Scarlet Witch
  , SomeEventCard chaosMagic
  , SomeEventCard hexBolt
  , SomeEventCard molecularDecay
  , SomeEventCard warpReality
  , SomeEventCard crisisAverted
  , SomeEventCard multitasking
  , SomeEventCard swiftRetribution
  , SomeEventCard turnTheTide
  , SomeEventCard spiritualMeditation
  , SomeEventCard browbeat
  , SomeEventCard lastStand
  , SomeEventCard baitAndSwitch
  , SomeEventCard recuperation

  -- Star-Lord
  , SomeEventCard daringEscape
  , SomeEventCard gutsyMove
  , SomeEventCard slidingShot
  , SomeEventCard airSupremacy
  , SomeEventCard blazeOfGlory
  , SomeEventCard diveBomb
  , SomeEventCard agileFlight
  , SomeEventCard everVigilant

  -- Gamora
  , SomeEventCard acrobaticMove
  , SomeEventCard crosscounter
  , SomeEventCard setThePace
  , SomeEventCard decisiveBlow
  , SomeEventCard forwardMomentum
  , SomeEventCard clobber
  , SomeEventCard planOfAttack
  , SomeEventCard firstHit
  , SomeEventCard impede
  , SomeEventCard hitAndRun
  , SomeEventCard pivotalMoment
  , SomeEventCard trueGrit

  -- Groot
  , SomeEventCard fruition
  , SomeEventCard iAmGroot
  , SomeEventCard iAmGrootExclaim
  , SomeEventCard rootStomp
  , SomeEventCard weAreGroot
  , SomeEventCard fightingFit
  , SomeEventCard floraAndFauna

  -- Rocket Raccoon
  , SomeEventCard iveGotAPlan
  , SomeEventCard reload
  , SomeEventCard schadenfreude
  , SomeEventCard lookingForTrouble

  -- The Galaxy's Most Wanted: The Market
  , SomeEventCard brainstorm
  , SomeEventCard byAnyMeans
  , SomeEventCard contingencyPlan
  , SomeEventCard inDefiance
  , SomeEventCard calculateTheOdds
  , SomeEventCard creativeSolution
  , SomeEventCard grapple
  , SomeEventCard wingIt
  , SomeEventCard closeCall
  , SomeEventCard defyDanger
  , SomeEventCard inHarmsWay
  , SomeEventCard takeTheFightToThem
  , SomeEventCard ardentResolve
  , SomeEventCard onrush
  , SomeEventCard safeguard
  , SomeEventCard sureGamble
  , SomeEventCard grandStrategy
  , SomeEventCard powerUnleashed
  , SomeEventCard triedAndTrue
  , SomeEventCard tripleThreat
  -- Drax
  , SomeEventCard fightMeCoward
  , SomeEventCard intimidation
  , SomeEventCard knifeLeap
  , SomeEventCard parry
  , SomeEventCard payback
  , SomeEventCard deflection
  , SomeEventCard hardKnocks
  , SomeEventCard leadingBlow
  , SomeEventCard subdue
  , SomeEventCard bringIt
  , SomeEventCard thinkFast

  -- Venom
  , SomeEventCard behindEnemyLines
  , SomeEventCard graspingTendrils
  , SomeEventCard lockedAndLoaded
  , SomeEventCard runAndGun
  , SomeEventCard savageAttack
  , SomeEventCard scareTactic
  , SomeEventCard makingAnEntrance
  , SomeEventCard fusillade
  , SomeEventCard welcomeAboard
  , SomeEventCard shakeItOff

  -- Spectrum
  , SomeEventCard gammaBlast
  , SomeEventCard photonSpeed
  , SomeEventCard pulsarShield
  , SomeEventCard speedOfLight
  , SomeEventCard massAttack

  -- Adam Warlock
  , SomeEventCard karmicBlast
  , SomeEventCard cosmicAwareness
  , SomeEventCard quantumMagic
  , SomeEventCard inBetweener
  , SomeEventCard magicAttack
  , SomeEventCard livingTribunal
  , SomeEventCard zoneOfSilence
  , SomeEventCard eternity
  , SomeEventCard summoningSpell
  , SomeEventCard theGardener
  , SomeEventCard shieldSpell
  , SomeEventCard combatReady
  , SomeEventCard lethalIntent
  , SomeEventCard oneWayOrAnother
  , SomeEventCard brainsOverBrawn
  , SomeEventCard daughtersOfThanos
  , SomeEventCard repulsorBeam
  , SomeEventCard targetedStrike
  , SomeEventCard scorchedEarth
  , SomeEventCard fullAuto
  , SomeEventCard sneakAttack
  , SomeEventCard saveTheDay
  , SomeEventCard goDownSwinging
  , SomeEventCard twoAgainstTheWorld
  , SomeEventCard asOne
  , SomeEventCard standTogether
  , SomeEventCard visitValhalla
  , SomeEventCard chooserOfTheSlain
  , SomeEventCard shieldmaiden
  , SomeEventCard haveAtThee
  , SomeEventCard quickStrike
  , SomeEventCard smashTheProblem
  , SomeEventCard theBestDefense
  , SomeEventCard godlikeStamina
  , SomeEventCard problemSolvers
  , SomeEventCard cosmicAlliance
  , SomeEventCard solarBeam
  , SomeEventCard superdenseStrike
  , SomeEventCard justPassingThrough
  , SomeEventCard phaseDisruption
  , SomeEventCard massIncrease
  , SomeEventCard defiance
  , SomeEventCard reboot
  , SomeEventCard joiningForces
  , SomeEventCard meditation
  , SomeEventCard ghostKick
  , SomeEventCard parentalGuidance
  , SomeEventCard phantomFlip
  , SomeEventCard pirouetteAndPunch
  , SomeEventCard webBinding
  , SomeEventCard jumpFlip
  , SomeEventCard returnTheFavor
  , SomeEventCard whatDoesntKillMe
  , SomeEventCard acrossTheSpiderVerse
  , SomeEventCard youngLove
  , SomeEventCard arachnobatics
  , SomeEventCard doubleLife
  , SomeEventCard swingIn
  , SomeEventCard webShot
  , SomeEventCard homelandIntervention
  , SomeEventCard globalLogistics
  , SomeEventCard forcefieldProjection
  , SomeEventCard lightspeedFlight
  , SomeEventCard potShot
  , SomeEventCard unleashNovaForce
  , SomeEventCard pitchback
  , SomeEventCard noQuarter
  , SomeEventCard oneByOne
  , SomeEventCard yawAndRoll
  , SomeEventCard flyOver
  , SomeEventCard photonBeam
  , SomeEventCard newAndImproved
  , SomeEventCard sectorScan
  , SomeEventCard goAllOut
  , SomeEventCard pushAhead
  , SomeEventCard goForChampions
  , SomeEventCard hamItUp
  , SomeEventCard hogwashed
  , SomeEventCard iDontThinkSo
  , SomeEventCard petulantPig
  , SomeEventCard swingingWebPig
  , SomeEventCard evenTheOdds
  , SomeEventCard allSystemsGo
  , SomeEventCard rapidDeployment
  , SomeEventCard webTrap
  , SomeEventCard repurpose
  , SomeEventCard thwipThwip
  , SomeEventCard limitlessStamina
  , SomeEventCard madeOfRage
  , SomeEventCard steelFist
  , SomeEventCard bulletproofProtector
  , SomeEventCard armorUp
  , SomeEventCard powerfulPunch
  , SomeEventCard mutantProtectors
  , SomeEventCard shadowAndSteel
  , SomeEventCard shadowcatSurprise
  , SomeEventCard phasedStrike
  , SomeEventCard airwalk
  , SomeEventCard quickShift
  , SomeEventCard teamStrike
  , SomeEventCard telekineticAttack
  , SomeEventCard psychicBlast
  , SomeEventCard telepathicTrickery
  , SomeEventCard phoenixFirebird
  , SomeEventCard psychicManipulation
  , SomeEventCard mutantPeacekeepers
  , SomeEventCard psychicRapport
  , SomeEventCard psychicAssault
  , SomeEventCard psychicMisdirection
  , SomeEventCard psychicKicker
  , SomeEventCard soulSisters
  ]

lookupEvent :: CardCode -> IdentityId -> EventId -> Event
lookupEvent cardCode identityId eventId = case lookup cardCode allEvents of
  Just (SomeEventCard a) -> Event $ buildCard a (identityId, eventId)
  Nothing -> error $ "Invalid card code for event " <> show cardCode
