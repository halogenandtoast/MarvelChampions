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
  [ SomeEventCard backflip
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
  ]

lookupEvent :: CardCode -> IdentityId -> EventId -> Event
lookupEvent cardCode identityId eventId = case lookup cardCode allEvents of
  Just (SomeEventCard a) -> Event $ buildCard a (identityId, eventId)
  Nothing -> error $ "Invalid card code for event " <> show cardCode
