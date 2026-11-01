module Marvel.Event.Cards where

import Marvel.Window.Types qualified as NcWindow

import Marvel.Trait.Types qualified as NcTrait

import Marvel.Resource.Types qualified as NcResource

import Marvel.Name qualified as NcName ()

import Marvel.Matchers qualified as NcMatch

import Marvel.Keyword qualified as NcKeyword

import Marvel.EncounterSet qualified as NcSet ()

import Marvel.Criteria qualified as NcCriteria

import Marvel.Cost.Types qualified as NcCost

import Marvel.Card.Def qualified as NcCard

import Marvel.Boost qualified as NcBoost ()

import Marvel.Aspect qualified as NcAspect

import Marvel.Ability.Types qualified as NcAbility

import Marvel.Jubilee.Cards qualified as Jubilee

import Marvel.Iceman.Cards qualified as IcemanPack
import Marvel.Ability.Types qualified as MgAbility
import Marvel.Aspect qualified as MgAspect
import Marvel.Boost qualified as MgBoost ()
import Marvel.Card.Def qualified as MgCard
import Marvel.Cost.Types qualified as MgCost
import Marvel.Criteria qualified as MgCriteria
import Marvel.EncounterSet qualified as MgSet ()
import Marvel.Keyword qualified as MgKeyword ()
import Marvel.Matchers qualified as MgMatch
import Marvel.Name qualified as MgName ()
import Marvel.Resource.Types qualified as MgResource
import Marvel.Trait qualified as MgTrait
import Marvel.Window.Types qualified as MgWindow

import Marvel.Matchers qualified as AoaMM
import Marvel.Window.Types qualified as AoaMW
import Marvel.Ability.Types qualified as AoaMAbility
import Marvel.Aspect qualified as AoaMA
import Marvel.Resource.Types qualified as AoaMR
import Marvel.Trait qualified as AoaMT
import Marvel.Ability.Types qualified as BpAbility
import Marvel.Aspect qualified as BpAspect
import Marvel.Card.Def qualified as BpCard
import Marvel.Cost.Types qualified as BpCost
import Marvel.Criteria qualified as BpCriteria
import Marvel.Matchers qualified as BpMatch
import Marvel.Resource.Types qualified as BpResource
import Marvel.Trait qualified as BpTrait
import Marvel.Window.Types qualified as BpWindow

import Marvel.Ability.Types qualified as DpAbility
import Marvel.Aspect qualified as DpAspect
import Marvel.Boost qualified as DpBoost ()
import Marvel.Card.Def qualified as DpCard
import Marvel.Cost.Types qualified as DpCost
import Marvel.Criteria qualified as DpCriteria
import Marvel.EncounterSet qualified as DpSet ()
import Marvel.Keyword qualified as DpKeyword
import Marvel.Matchers qualified as DpMatch
import Marvel.Name qualified as DpName
import Marvel.Resource.Types qualified as DpResource
import Marvel.Trait qualified as DpTrait
import Marvel.Window.Types qualified as DpWindow
import Marvel.Damage qualified as DpDamage

import Marvel.Ability.Types qualified as AnAbility
import Marvel.Aspect qualified as AnAspect
import Marvel.Boost qualified as AnBoost ()
import Marvel.Card.Def qualified as AnCard
import Marvel.Cost.Types qualified as AnCost
import Marvel.Criteria qualified as AnCriteria
import Marvel.EncounterSet qualified as AnSet ()
import Marvel.Keyword qualified as AnKeyword
import Marvel.Matchers qualified as AnMatch
import Marvel.Name qualified as AnName ()
import Marvel.Resource.Types qualified as AnResource
import Marvel.Trait qualified as AnTrait
import Marvel.Window.Types qualified as AnWindow

import Marvel.Ability.Types qualified as PsyAbility
import Marvel.Aspect qualified as PsyAspect
import Marvel.Boost qualified as PsyBoost ()
import Marvel.Card.Def qualified as PsyCard
import Marvel.Cost.Types qualified as PsyCost
import Marvel.Criteria qualified as PsyCriteria
import Marvel.EncounterSet qualified as PsySet ()
import Marvel.Keyword qualified as PsyKeyword
import Marvel.Matchers qualified as PsyMatch
import Marvel.Name qualified as PsyName ()
import Marvel.Resource.Types qualified as PsyResource
import Marvel.Trait qualified as PsyTrait
import Marvel.Window.Types qualified as PsyWindow

import Marvel.Boost ()
import Marvel.EncounterSet qualified as NES ()
import Marvel.Aspect qualified as NEA
import Marvel.Resource.Types qualified as NER
import Marvel.Keyword ()
import Marvel.Trait qualified as NET
import Marvel.Trait qualified as NE
import Marvel.Resource qualified as NE
import Marvel.Keyword ()
import Marvel.Boost ()
import Marvel.Aspect qualified as NE
import Marvel.Card.Def ()
import Marvel.Ability.Types qualified as NEAbility

import Marvel.Keyword
import Marvel.Boost ()
import Marvel.EncounterSet
import Marvel.Prelude

import Marvel.Cost.Types

import Marvel.Ability.Types (AbilityType(..))
import Marvel.Ability.Types qualified as Ability
import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Status (StatusCard (StunnedStatus, ConfusedStatus))
import Marvel.Damage
import Marvel.GameValue.Types
import Marvel.Matchers
import Marvel.Name
import Marvel.Resource
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Upgrade.Cards qualified as Upgrade
import Marvel.Window

allEvents :: HashMap CardCode CardDef
allEvents = fromList $ concatMap
  toCardCodePairs
  [ nc48007, nc48008, nc48009, nc48010, nc48011, nc48016, nc48018, nc48031, nc48032
  , Jubilee.jubilee47006, Jubilee.jubilee47007a, Jubilee.jubilee47007b, Jubilee.jubilee47007c, Jubilee.jubilee47008a, Jubilee.jubilee47008b, Jubilee.jubilee47008c, Jubilee.jubilee47009, Jubilee.jubilee47014, Jubilee.jubilee47015, Jubilee.jubilee47021, Jubilee.jubilee47022, Jubilee.jubilee47028, Jubilee.jubilee47029
  , arcticAttack
  , iceBlast
  , chillOut
  , surpriseMove
  , takeThat
  , electromagneticBlast, metalShards, magneticMissile, squaredOff, nobleSacrifice, youGotThis, faceThePast
  , basicSpell
  , fullBodyCharge
  , magicBarrier
  , soulStrike
  , exorcism
  , steppingDisc
  , scrying
  , concussiveBlast, commandAuthority, energyConversion, sideBySide, suitUp
  , exhaustingPersonality, maximumEffort, metaknowledge, yooHoo, thisCardIsFire, barelyAScratch, cutupper, daBomb, getRageY, iGotThis, notMyResponsibility, poolInspection, breakTime, getInFrontOfMe, mulligan
  , animalInstinct, clawMastery, regenerativeLongevity, sisterlyBond, criticalHit, predictablePloy, anticipatedAttack
  , adaptivePlumage, aerialAgility, metamorphosis, naturalFlight, razorDive, aerialIntervention, taunt, bombsAway, flyingFormation
  , flurryOfBlades, mentalDetection, psionicRedirect, telepathicSuggestion, upsideTheHead, directedForce, soaringHearts, psiBowAttack, psiFlailStrike
  , thePosse
  , teamInvestigation
  , rightPlaceRightTime
  , luckBeaLady
  , aGoodWorkout
  , cable40002, cable40003, cable40004, cable40005, cable40017, cable40026
  , goinRogue, southernCross, energyTransfer, bulletproofBelle, superpowerAdaptation, notToday
  , chargedCard, royalFlush, naturalAgility, creoleCharmer, breakingAndEntering, beautyAndTheThief, mutantEducation, xMenInstruction
  , weatherGoddess, torrentialRain, lightningBolt, flashFreeze, blastOfWind, toMeMyXMen
  , fullBlast, ricochetBeam, tacticalBrilliance, gameTime
  , berserkerBarrage, sliceAndDice, lungingStrike, trackByScent, regenerativeHealing, outtaMyWay, precisionStrike, fastballSpecial
  , backflip
  , enhancedSpiderSense
  , swingingWebKick
  , crisisInterdiction
  , photonicBlast
  , gammaSlam
  , groundStomp
  , legalPractice
  , oneTwoPunch
  , splitPersonality
  , repulsorBlast
  , supersonicPunch
  , ancestralKnowledge
  , wakandaForeverA
  , wakandaForeverB
  , wakandaForeverC
  , wakandaForeverD
  , chaseThemDown
  , relentlessAssault
  , uppercut
  , forJustice
  , greatResponsibility
  , getReady
  , leadFromTheFront
  , makeTheCall
  , counterPunch
  , getBehindMe
  , emergency
  , firstAid
  , haymaker
  , fearlessDetermination
  , heroicStrike
  , shieldBlock
  , shieldToss
  , avengersAssemble
  , strengthInNumbers
  , expertDefense
  , bigHands
  , sneakBy
  , wiggleRoom
  , preemptiveStrike
  , tackle
  , melee
  , concussiveBlow
  , moraleBoost
  , covertOps
  , danceOfDeath
  , stealthStrike
  , defenderOfTheNineRealms
  , forAsgard
  , hammerThrow
  , lightningStrike
  , getOverHere
  , meanSwing
  , invulnerability
  , teamwork
  , secondWind
  , astralProjection
  , magicBlast
  , masterOfTheMysticArts
  , mysticalStudies
  , protectiveWard
  , desperateDefense
  , momentumShift
  , warning
  , crimsonBandsOfCyttorak
  , imagesOfIkonn
  , sevenRingsOfRaggadorr
  , vaporsOfValtorr
  , windsOfWatoomb
  , skilledStrike
  , foiled
  , crushingBlow
  , hulkSmash
  , subOrbitalLeap
  , thunderclap
  , unstoppableForce
  , dropKick
  , toeToToe
  , youllPayForThat
  , toTheRescue
  , inspiringPresence
  -- The Green Goblin

  -- The Wrecking Crew

  -- The Rise of Red Skull
  , sonicArrow
  , explosiveArrow
  , electricArrow
  , cableArrow
  , vibraniumArrow
  , readyForAction
  , earthsMightiestHeroes
  , venomBlast
  , pheromones
  , contaminantImmunity
  , inconspicuous
  , selfPropelledGlide
  , pressTheAdvantage
  , piercingStrike
  , clearTheArea

  -- The Once and Future Kang


  -- Ant-Man
  , giantStomp
  , hiveMind
  , resize
  , callForAid
  , moxie
  , swarmTactics
  , momentOfTriumph
  , layDownTheLaw
  , musterCourage
  , assessTheSituation

  -- Wasp
  , giantHelp
  , pinpointStrike
  , rapidGrowth
  , waspSting
  , intoTheFray
  , surpriseAttack
  , athleticConditioning
  , runningInterference
  , allForOne
  , perseverance

  -- Quicksilver
  , alwaysBeRunning
  , doubleTime
  , maximumVelocity
  , speedCyclone
  , neverBackDown
  , sideStep
  , orderAndChaos
  , unitedWeStand
  , beatEmUp

  -- Scarlet Witch
  , chaosMagic
  , hexBolt
  , molecularDecay
  , warpReality
  , crisisAverted
  , multitasking
  , swiftRetribution
  , turnTheTide
  , spiritualMeditation
  , browbeat
  , lastStand
  , baitAndSwitch
  , recuperation

  -- Star-Lord
  , daringEscape
  , gutsyMove
  , slidingShot
  , airSupremacy
  , blazeOfGlory
  , diveBomb
  , agileFlight
  , everVigilant

  -- Gamora
  , acrobaticMove
  , crosscounter
  , setThePace
  , decisiveBlow
  , forwardMomentum
  , clobber
  , planOfAttack
  , firstHit
  , impede
  , hitAndRun
  , pivotalMoment
  , trueGrit

  -- Groot
  , fruition
  , iAmGroot
  , iAmGrootExclaim
  , rootStomp
  , weAreGroot
  , fightingFit
  , floraAndFauna

  -- Rocket Raccoon
  , iveGotAPlan
  , reload
  , schadenfreude
  , lookingForTrouble

  -- The Galaxy's Most Wanted: The Market
  , brainstorm
  , byAnyMeans
  , contingencyPlan
  , inDefiance
  , calculateTheOdds
  , creativeSolution
  , grapple
  , wingIt
  , closeCall
  , defyDanger
  , inHarmsWay
  , takeTheFightToThem
  , ardentResolve
  , onrush
  , safeguard
  , sureGamble
  , grandStrategy
  , powerUnleashed
  , triedAndTrue
  , tripleThreat
  -- Drax
  , fightMeCoward
  , intimidation
  , knifeLeap
  , parry
  , payback
  , deflection
  , hardKnocks
  , leadingBlow
  , subdue
  , bringIt
  , thinkFast

  -- Venom
  , behindEnemyLines
  , graspingTendrils
  , lockedAndLoaded
  , runAndGun
  , savageAttack
  , scareTactic
  , makingAnEntrance
  , fusillade
  , welcomeAboard
  , shakeItOff

  -- Spectrum
  , gammaBlast
  , photonSpeed
  , pulsarShield
  , speedOfLight
  , massAttack

  -- Adam Warlock
  , karmicBlast
  , cosmicAwareness
  , quantumMagic
  , inBetweener
  , magicAttack
  , livingTribunal
  , zoneOfSilence
  , eternity
  , summoningSpell
  , theGardener
  , shieldSpell
  , combatReady
  , lethalIntent
  , oneWayOrAnother
  , brainsOverBrawn
  , daughtersOfThanos
  , repulsorBeam
  , targetedStrike
  , scorchedEarth
  , fullAuto
  , sneakAttack
  , saveTheDay
  , goDownSwinging
  , twoAgainstTheWorld
  , asOne
  , standTogether
  , visitValhalla
  , chooserOfTheSlain
  , shieldmaiden
  , haveAtThee
  , quickStrike
  , smashTheProblem
  , theBestDefense
  , godlikeStamina
  , problemSolvers
  , cosmicAlliance
  , solarBeam
  , superdenseStrike
  , justPassingThrough
  , phaseDisruption
  , massIncrease
  , defiance
  , reboot
  , joiningForces
  , meditation
  , ghostKick
  , parentalGuidance
  , phantomFlip
  , pirouetteAndPunch
  , webBinding
  , jumpFlip
  , returnTheFavor
  , whatDoesntKillMe
  , acrossTheSpiderVerse
  , youngLove
  , arachnobatics
  , doubleLife
  , swingIn
  , webShot
  , homelandIntervention
  , globalLogistics
  , forcefieldProjection
  , lightspeedFlight
  , potShot
  , unleashNovaForce
  , pitchback
  , noQuarter
  , oneByOne
  , yawAndRoll
  , flyOver
  , photonBeam
  , newAndImproved
  , sectorScan
  , goAllOut
  , pushAhead
  , goForChampions
  , hamItUp
  , hogwashed
  , iDontThinkSo
  , petulantPig
  , swingingWebPig
  , evenTheOdds
  , allSystemsGo
  , rapidDeployment
  , webTrap
  , repurpose
  , thwipThwip
  , limitlessStamina
  , madeOfRage
  , steelFist
  , bulletproofProtector
  , armorUp
  , powerfulPunch
  , mutantProtectors
  , shadowAndSteel
  , shadowcatSurprise
  , phasedStrike
  , airwalk
  , quickShift
  , teamStrike
  , telekineticAttack
  , psychicBlast
  , telepathicTrickery
  , phoenixFirebird
  , psychicManipulation
  , mutantPeacekeepers
  , psychicRapport
  , psychicAssault
  , psychicMisdirection
  , psychicKicker
  , soulSisters
  ]

event
  :: CardCode
  -> Name
  -> Int
  -> AbilityType
  -> [Trait]
  -> [Resource]
  -> Aspect
  -> CardDef
event code name cost aType traits resources aspect =
  baseEvent code name cost aType traits resources (Just aspect)

identityEvent
  :: CardCode -> Name -> Int -> AbilityType -> [Trait] -> [Resource] -> CardDef
identityEvent code name cost aType traits resources =
  baseEvent code name cost aType traits resources Nothing

basicEvent
  :: CardCode -> Name -> Int -> AbilityType -> [Trait] -> [Resource] -> CardDef
basicEvent code name cost aType traits resources =
  baseEvent code name cost aType traits resources (Just Neutral)

baseEvent
  :: CardCode
  -> Name
  -> Int
  -> AbilityType
  -> [Trait]
  -> [Resource]
  -> Maybe Aspect
  -> CardDef
baseEvent code name cost aType traits resources mAspect = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Just cost
  , cdTraits = fromList traits
  , cdKeywords = mempty
  , cdCardType = EventType
  , cdAbilityType = Just aType
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = mAspect
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdResources = map (PrintedResource, ) resources
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

backflip :: CardDef
backflip =
  (identityEvent "01003" "Backflip" 0 Interrupt [Defense, Skill] [Physical])
    { cdResponseWindow = Just (WouldTakeDamage You FromAttack AnyValue)
    , cdAbilitySubTypes = [Ability.Defense]
    }

enhancedSpiderSense :: CardDef
enhancedSpiderSense = (identityEvent
                        "01004"
                        "Enhanced Spider-Sense"
                        1
                        HeroInterrupt
                        [Superpower]
                        [Mental]
                      )
  { cdResponseWindow = Just
    (TreacheryRevealed When AnyTreachery RevealedFromEncounterDeck)
  , cdCriteria = CanCancelRevealedTreachery
  }

swingingWebKick :: CardDef
swingingWebKick = (identityEvent
                    "01005"
                    "Swinging Web Kick"
                    3
                    HeroAction
                    [Aerial, Attack, Superpower]
                    [Mental]
                  )
  { cdAbilitySubTypes = [Ability.Attack]
  }

crisisInterdiction :: CardDef
crisisInterdiction = (identityEvent
                    "01012"
                    "Crisis Interdiction"
                    2
                    HeroAction
                    [Thwart]
                    [Energy]
                  )
  { cdAbilitySubTypes = [Ability.Thwart]
  }

photonicBlast :: CardDef
photonicBlast = (identityEvent
                  "01013"
                  "Photonic Blast"
                  3
                  HeroAction
                  [Attack, Superpower]
                  [Physical]
                )
  { cdAbilitySubTypes = [Ability.Attack]
  }

gammaSlam :: CardDef
gammaSlam =
  (identityEvent "01021" "Gamma Slam" 4 HeroAction [Attack, Superpower] [Mental]
    )
    { cdAbilitySubTypes = [Ability.Attack]
    }

groundStomp :: CardDef
groundStomp =
  identityEvent "01022" "Ground Stomp" 2 HeroAction [Superpower] [Mental]

legalPractice :: CardDef
legalPractice = (identityEvent
                  "01023"
                  "Legal Practice"
                  0
                  AlterEgoAction
                  [Skill, Thwart]
                  [Physical]
                )
  { cdAbilitySubTypes = [Ability.Thwart]
  , cdAbilityCost = DiscardHandCardsCost 1 (Just 5)
  }

oneTwoPunch :: CardDef
oneTwoPunch =
  (identityEvent "01024" "One-Two Punch" 1 Response [Skill] [Physical])
    { cdResponseWindow = Just (MakesBasicAttack After You)
    }

splitPersonality :: CardDef
splitPersonality =
  identityEvent "01025" "Split Personality" 3 Action [] [Energy]

repulsorBlast :: CardDef
repulsorBlast = (identityEvent
                  "01031"
                  "Repulsor Blast"
                  1
                  HeroAction
                  [Attack, Superpower]
                  [Physical]
                )
  { cdCriteria = canAttack
  , cdAbilitySubTypes = [Ability.Attack]
  }

supersonicPunch :: CardDef
supersonicPunch =
  (identityEvent "01032" "Supersonic Punch" 2 HeroAction [Attack] [Energy])
    { cdCriteria = canAttack
    , cdAbilitySubTypes = [Ability.Attack]
    }

ancestralKnowledge :: CardDef
ancestralKnowledge =
  identityEvent "01042" "Ancestral Knowledge" 1 AlterEgoAction [] [Mental]

wakandaForeverA :: CardDef
wakandaForeverA =
  identityEvent "01043a" "Wakanda Forever!" 1 HeroAction [Tactic] [Energy]

wakandaForeverB :: CardDef
wakandaForeverB =
  identityEvent "01043b" "Wakanda Forever!" 1 HeroAction [Tactic] [Mental]

wakandaForeverC :: CardDef
wakandaForeverC =
  identityEvent "01043c" "Wakanda Forever!" 1 HeroAction [Tactic] [Physical]

wakandaForeverD :: CardDef
wakandaForeverD =
  identityEvent "01043d" "Wakanda Forever!" 1 HeroAction [Tactic] [Wild]

chaseThemDown :: CardDef
chaseThemDown = reprintedAs ["06013", "16041", "28011"] $
  (event "01052" "Chase Them Down" 0 Response [Thwart] [Mental] Aggression)
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SchemeExists AnyScheme
    , cdResponseWindow = Just $ EnemyDefeated After AnyEnemy $ AttackFromPlayer
      You
    }

relentlessAssault :: CardDef
relentlessAssault = reprintedAs ["16044"] $
  (event "01053" "Relentless Assault" 2 HeroAction [Attack] [Energy] Aggression)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttackMinion
    }

uppercut :: CardDef
uppercut = reprintedAs ["18014", "21044"] $
  (event "01054" "Uppercut" 3 HeroAction [Attack] [Physical] Aggression)
    { cdAbilitySubTypes = [Ability.Attack]
    }

forJustice :: CardDef
forJustice = reprintedAs ["21049"] $
  (event "01060" "For Justice!" 2 HeroAction [Thwart] [Energy] Justice)
    { cdCriteria = SchemeExists ThwartableScheme
    , cdAbilitySubTypes = [Ability.Thwart]
    }

greatResponsibility :: CardDef
greatResponsibility = reprintedAs ["30015"] $
  (event "01061" "Great Responsibility" 0 HeroInterrupt [] [Mental] Justice)
    { cdResponseWindow = Just (ThreatWouldBePlaced AnyThreatSource AnyScheme)
    }

getReady :: CardDef
getReady = reprintedAs ["17016"] $ (event "01069" "Get Ready" 0 Action [] [Physical] Leadership)
  { cdCriteria = AllyExists ExhaustedAlly
  }

leadFromTheFront :: CardDef
leadFromTheFront = reprintedAs ["45018", "04018"] $
  event "01070" "Lead from the Front" 2 HeroAction [Tactic] [Energy] Leadership

makeTheCall :: CardDef
makeTheCall = deckLimited 3 $ reprintedAs ["03016", "21056", "23020"] $ (event "01071" "Make the Call" 0 Action [] [Mental] Leadership)
  { cdAbilityCost = TargetPrintedCost
  }

counterPunch :: CardDef
counterPunch = reprintedAs ["19014", "21062"] $
  (event "01077" "Counter-Punch" 0 Response [Attack] [Physical] Protection)
    { cdResponseWindow = Just (HeroDefended After You AttackableEnemy)
    , cdAbilitySubTypes = [Ability.Attack]
    }

getBehindMe :: CardDef
getBehindMe = deckLimited 3 $ reprintedAs ["05013", "26020"] $
  (event "01078" "Get Behind Me!" 1 HeroInterrupt [] [Mental] Protection)
    { cdResponseWindow =
        Just (TreacheryRevealed When AnyTreachery RevealedFromEncounterDeck)
    }

emergency :: CardDef
emergency = (basicEvent "01085" "Emergency" 0 Interrupt [Thwart] [Energy])
  { cdResponseWindow = Just (ThreatWouldBePlaced ThreatFromVillain AnyScheme)
  , cdAbilitySubTypes = [Ability.Thwart]
  }

firstAid :: CardDef
firstAid = deckLimited 3 $ reprintedAs ["12019", "22023"] (basicEvent "01086" "First Aid" 1 Action [] [Mental])
  { cdCriteria = CharacterExists CharacterWithAnyDamage
  }

haymaker :: CardDef
haymaker = (basicEvent "01087" "Haymaker" 2 HeroAction [Attack] [Energy])
  { cdAbilitySubTypes = [Ability.Attack]
  }

fearlessDetermination :: CardDef
fearlessDetermination =
  identityEvent "03003" "Fearless Determination" 0 HeroAction [Superpower] [Mental]

heroicStrike :: CardDef
heroicStrike =
  (identityEvent "03004" "Heroic Strike" 3 HeroAction [Attack] [Physical])
    { cdCriteria = canAttack
    , cdAbilitySubTypes = [Ability.Attack]
    }

-- | Exhausting the shield is part of its cost: it is played only with the shield ready.
shieldBlock :: CardDef
shieldBlock =
  (identityEvent "03005" "Shield Block" 0 Interrupt [Defense] [Mental])
    { cdResponseWindow = Just (WouldTakeDamage You FromAnyDamageSource AnyValue)
    , cdAbilitySubTypes = [Ability.Defense]
    , cdAbilityCost = ExhaustUpgradeCost (UpgradeIs Upgrade.captainAmericasShield)
    }

-- | Returning the shield is part of its cost: it is played only with the shield in play.
shieldToss :: CardDef
shieldToss =
  (identityEvent "03006" "Shield Toss" 0 HeroAction [Attack, Superpower] [Energy])
    { cdCriteria = canAttack
    , cdAbilitySubTypes = [Ability.Attack]
    , cdAbilityCost =
        DiscardHandCardsCost 0 Nothing <> ReturnUpgradeToHandCost (UpgradeIs Upgrade.captainAmericasShield)
    }

avengersAssemble :: CardDef
avengersAssemble =
  (event "03015" "Avengers Assemble!" 4 HeroAction [] [Energy] Leadership)
    { cdLimit = Just (MaxPerRound 1)
    }

strengthInNumbers :: CardDef
strengthInNumbers =
  (event "03017" "Strength in Numbers" 0 Action [] [Physical] Leadership)
    { cdAbilityCost = ExhaustAlliesCost 1 Nothing
    }

expertDefense :: CardDef
expertDefense =
  (event "03033" "Expert Defense" 0 HeroInterrupt [Defense] [Mental] Protection)
    { cdResponseWindow = Just (HeroDefended When You AnyEnemy)
    , cdAbilitySubTypes = [Ability.Defense]
    }

bigHands :: CardDef
bigHands =
  (identityEvent "05003" "Big Hands" 2 HeroAction [Attack, Superpower] [Physical])
    { cdAbilitySubTypes = [Ability.Attack]
    }

sneakBy :: CardDef
sneakBy =
  (identityEvent "05004" "Sneak By" 2 HeroAction [Skill, Thwart] [Mental])
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SchemeExists ThwartableScheme
    }

wiggleRoom :: CardDef
wiggleRoom =
  (identityEvent "05005" "Wiggle Room" 0 HeroInterrupt [Defense, Superpower] [Energy])
    { cdAbilitySubTypes = [Ability.Defense]
    , cdResponseWindow = Just (WouldTakeDamage You FromAnyDamageSource AnyValue)
    }

preemptiveStrike :: CardDef
preemptiveStrike =
  reprintedAs ["38015"] $ (event "05014" "Preemptive Strike" 1 HeroInterrupt [Defense] [Energy] Protection)
    { cdAbilitySubTypes = [Ability.Defense]
    , cdResponseWindow = Just (BoostCardFlipped When (VillainEnemy <> AttackingEnemy))
    }

tackle :: CardDef
tackle =
  (event "05015" "Tackle" 3 HeroAction [Attack] [Physical] Protection)
    { cdAbilitySubTypes = [Ability.Attack]
    }

melee :: CardDef
melee =
  (event "05030" "Melee" 3 HeroAction [Attack] [Mental] Aggression)
    { cdAbilitySubTypes = [Ability.Attack]
    }

concussiveBlow :: CardDef
concussiveBlow = reprintedAs ["41014"] $
  (event "05031" "Concussive Blow" 3 HeroAction [Attack] [Physical] Justice)
    { cdAbilitySubTypes = [Ability.Attack]
    }

moraleBoost :: CardDef
moraleBoost = reprintedAs ["29019"] $ event "05032" "Morale Boost" 1 HeroAction [] [Energy] Leadership
crushingBlow :: CardDef
crushingBlow =
  (identityEvent "10002" "Crushing Blow" 1 HeroAction [Attack] [Physical])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = SpendOnly Physical
    }

hulkSmash :: CardDef
hulkSmash =
  (identityEvent "10003" "Hulk Smash" 3 HeroInterrupt [Superpower] [Physical])
    { cdResponseWindow = Just (MakesBasicAttack When You)
    }

subOrbitalLeap :: CardDef
subOrbitalLeap =
  (identityEvent "10004" "Sub-Orbital Leap" 3 HeroAction [Superpower, Thwart] [Physical])
    { cdAbilitySubTypes = [Ability.Thwart]
    }

thunderclap :: CardDef
thunderclap = identityEvent "10005" "Thunderclap" 3 HeroAction [Superpower] [Physical]

unstoppableForce :: CardDef
unstoppableForce = identityEvent "10006" "Unstoppable Force" 2 HeroAction [] [Physical]

dropKick :: CardDef
dropKick =
  (event "10014" "Drop Kick" 3 HeroAction [Attack] [Physical] Aggression)
    { cdAbilitySubTypes = [Ability.Attack]
    }

toeToToe :: CardDef
toeToToe = reprintedAs ["32046"] $
  (event "10015" "Toe to Toe" 1 HeroAction [Attack] [Mental] Aggression)
    { cdAbilitySubTypes = [Ability.Attack]
    }

youllPayForThat :: CardDef
youllPayForThat =
  (event "10016" "\"You'll Pay for That!\"" 1 HeroResponse [Thwart] [Energy] Aggression)
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdResponseWindow = Just (EnemyAttacked After VillainEnemy You)
    }

toTheRescue :: CardDef
toTheRescue =
  (basicEvent "10019" "To the Rescue!" 2 HeroAction [Thwart] [Physical])
    { cdAbilitySubTypes = [Ability.Thwart]
    }

inspiringPresence :: CardDef
inspiringPresence =
  (event "10030" "Inspiring Presence" 1 HeroAction [] [Mental] Leadership)
    { cdCriteria = SelfMatches (IdentityWithTrait Avenger) <> AllyExists AnyAlly
    }

covertOps :: CardDef
covertOps = (identityEvent "08003" "Covert Ops" 3 Action [Thwart] [Mental])
  { cdAbilitySubTypes = [Ability.Thwart]
  }

-- | Not labeled: each of its three attacks is one (see the event).
danceOfDeath :: CardDef
danceOfDeath =
  identityEvent "08004" "Dance of Death" 3 HeroAction [Attack] [Energy]

stealthStrike :: CardDef
stealthStrike = reprintedAs ["37014"] $ (event "08013" "Stealth Strike" 3 HeroAction [Attack] [Mental] Justice)
  { cdAbilitySubTypes = [Ability.Attack]
  }

defenderOfTheNineRealms :: CardDef
defenderOfTheNineRealms =
  (identityEvent "06003" "Defender of the Nine Realms" 0 HeroAction [Thwart] [Physical])
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdAbilityCost = DiscardEncounterCardsUntilMinionCost
    }

forAsgard :: CardDef
forAsgard = identityEvent "06004" "For Asgard!" 1 AlterEgoAction [] [Physical]

-- | "Exhaust Mjolnir" is part of its cost: Mjolnir must be in play and ready.
hammerThrow :: CardDef
hammerThrow =
  (identityEvent "06005" "Hammer Throw" 3 HeroAction [Attack, Superpower] [Energy])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    , cdAbilityCost = ExhaustUpgradeCost (UpgradeIs Upgrades.mjolnir)
    }

lightningStrike :: CardDef
lightningStrike =
  (identityEvent "06006" "Lightning Strike" 1 HeroAction [Superpower] [Mental])
    { cdAbilityCost = DynamicResourceCost (Just Energy)
    }

getOverHere :: CardDef
getOverHere =
  (event "06014" "Get Over Here!" 0 HeroAction [Attack] [Energy] Aggression)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttackMinion
    }

-- | "Exhaust a Weapon upgrade on your hero" is part of its cost.
meanSwing :: CardDef
meanSwing =
  reprintedAs ["35019"] $ (event "06015" "Mean Swing" 0 HeroInterrupt [Skill] [Physical] Aggression)
    { cdResponseWindow = Just (MakesBasicAttack When You)
    , cdCriteria = SelfMatches You
    , cdAbilityCost = ExhaustUpgradeCost (UpgradeWithTrait Weapon)
    }

invulnerability :: CardDef
invulnerability = basicEvent "06021" "Invulnerability" 3 HeroAction [Superpower] [Physical]

-- | "Exhaust an ally you control" is part of its cost.
teamwork :: CardDef
teamwork =
  reprintedAs ["33017"] $ (event "06032" "Teamwork" 0 HeroInterrupt [Tactic] [Energy] Leadership)
    { cdResponseWindow = Just (WindowOneOf [MakesBasicAttack When You, MakesBasicThwart When You])
    , cdCriteria = SelfMatches You
    , cdAbilityCost = ExhaustAlliesCost 1 (Just 1)
    }

secondWind :: CardDef
secondWind = event "06033" "Second Wind" 3 Action [] [Mental] Protection

astralProjection :: CardDef
astralProjection =
  (identityEvent "09003" "Astral Projection" 2 HeroAction [Spell, Thwart] [Mental])
    { cdAbilitySubTypes = [Ability.Thwart]
    }

magicBlast :: CardDef
magicBlast =
  (identityEvent "09004" "Magic Blast" 3 HeroAction [Attack, Spell] [Energy])
    { cdCriteria = canAttack
    , cdAbilitySubTypes = [Ability.Attack]
    }

masterOfTheMysticArts :: CardDef
masterOfTheMysticArts =
  identityEvent "09005" "Master of the Mystic Arts" 1 HeroAction [] [Mental]

mysticalStudies :: CardDef
mysticalStudies =
  identityEvent "09006" "Mystical Studies" 1 AlterEgoAction [] [Physical]

protectiveWard :: CardDef
protectiveWard =
  (identityEvent "09007" "Protective Ward" 1 HeroInterrupt [Spell] [Physical])
    { cdResponseWindow =
        Just (TreacheryRevealed When AnyTreachery RevealedFromEncounterDeck)
    , cdCriteria = CanCancelRevealedTreachery
    }

desperateDefense :: CardDef
desperateDefense =
  reprintedAs ["16013"] $ (event "09015" "Desperate Defense" 1 HeroInterrupt [Defense] [Energy] Protection)
    { cdResponseWindow = Just (HeroDefended When You AnyEnemy)
    , cdAbilitySubTypes = [Ability.Defense]
    }

-- | healing 2 damage from your hero is its cost, so it is paid only in full
momentumShift :: CardDef
momentumShift =
  (event "09016" "Momentum Shift" 2 HeroAction [Attack] [Physical] Protection)
    { cdCriteria = canAttack
    , cdAbilitySubTypes = [Ability.Attack]
    , cdAbilityCost = HealCost 2
    }

-- | as errata'd: no longer a defense, nor a Defense card
warning :: CardDef
warning = (basicEvent "09021" "Warning" 0 Interrupt [] [Physical])
  { cdResponseWindow = Just (WouldTakeDamage HeroIdentity FromAnyDamageSource AnyValue)
  }

-- | An Invocation card: in Doctor Strange's Invocation deck, never his player deck, its
-- "Special" resolved without it being played.
invocation :: CardCode -> Name -> Int -> CardDef
invocation code name cost = identityEvent code name cost Special [Invocation] []

crimsonBandsOfCyttorak :: CardDef
crimsonBandsOfCyttorak = invocation "09032" "Crimson Bands of Cyttorak" 2

imagesOfIkonn :: CardDef
imagesOfIkonn = invocation "09033" "Images of Ikonn" 1

sevenRingsOfRaggadorr :: CardDef
sevenRingsOfRaggadorr = invocation "09034" "Seven Rings of Raggadorr" 1

vaporsOfValtorr :: CardDef
vaporsOfValtorr = invocation "09035" "Vapors of Valtorr" 0

windsOfWatoomb :: CardDef
windsOfWatoomb = invocation "09036" "Winds of Watoomb" 0

skilledStrike :: CardDef
skilledStrike =
  (event "09037" "Skilled Strike" 0 Interrupt [Skill] [Physical] Aggression)
    { cdResponseWindow = Just (MakesBasicAttack When You)
    }

foiled :: CardDef
foiled = (event "09038" "Foiled!" 0 Interrupt [] [Energy] Justice)
  { -- boost cards are turned faceup only as an enemy attacks or schemes
    cdResponseWindow = Just (BoostCardFlipped When $ NotEnemy AttackingEnemy)
  }

-- | "Exhaust Hawkeye's Bow" is part of an Arrow event's cost: the bow in play and ready.
arrow :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> CardDef
arrow code name cost traits resources =
  (identityEvent code name cost HeroAction (Arrow : traits) resources)
    { cdAbilityCost = ExhaustUpgradeCost (UpgradeIs Upgrades.hawkeyesBow)
    }

arrowAttack :: CardCode -> Name -> Int -> [Resource] -> CardDef
arrowAttack code name cost resources =
  (arrow code name cost [Attack] resources)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

sonicArrow :: CardDef
sonicArrow = arrowAttack "04005" "Sonic Arrow" 2 [Mental]

explosiveArrow :: CardDef
explosiveArrow = arrow "04006" "Explosive Arrow" 1 [] [Physical]

electricArrow :: CardDef
electricArrow = arrowAttack "04007" "Electric Arrow" 2 [Energy]

cableArrow :: CardDef
cableArrow =
  (arrow "04008" "Cable Arrow" 1 [Thwart] [Physical])
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SchemeExists ThwartableSchemeIgnoringCrisis
    }

vibraniumArrow :: CardDef
vibraniumArrow = arrowAttack "04009" "Vibranium Arrow" 2 [Energy]

readyForAction :: CardDef
readyForAction =
  (event "04017" "Ready for Action" 1 HeroAction [Tactic] [Mental] Leadership)
    { cdCriteria = AllyExists (AllyControlledBy You)
    }

-- | "Exhaust an Avenger character you control" is part of its cost.
earthsMightiestHeroes :: CardDef
earthsMightiestHeroes =
  (basicEvent "04022" "Earth's Mightiest Heroes" 0 HeroAction [] [Energy])
    { cdAbilityCost = ExhaustCharacterWithTraitCost Avenger
    }

venomBlast :: CardDef
venomBlast =
  (event "04035" "Venom Blast" 2 HeroAction [Attack, Superpower] [Energy] Aggression)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

pheromones :: CardDef
pheromones = event "04036" "Pheromones" 2 HeroAction [Superpower] [Physical] Leadership

contaminantImmunity :: CardDef
contaminantImmunity = event "04037" "Contaminant Immunity" 2 HeroAction [Superpower] [Energy] Protection

inconspicuous :: CardDef
inconspicuous =
  (event "04038" "Inconspicuous" 1 HeroAction [Skill, Thwart] [Energy] Justice)
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SchemeExists ThwartableScheme
    }

selfPropelledGlide :: CardDef
selfPropelledGlide = identityEvent "04039" "Self-Propelled Glide" 1 HeroAction [Superpower] [Mental]

pressTheAdvantage :: CardDef
pressTheAdvantage =
  (event "04043" "Press the Advantage" 1 HeroAction [Attack] [Mental] Aggression)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

piercingStrike :: CardDef
piercingStrike =
  (event "04044" "Piercing Strike" 2 HeroAction [Attack] [Physical] Aggression)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

clearTheArea :: CardDef
clearTheArea =
  (event "04049" "Clear the Area" 1 HeroAction [Thwart] [Mental] Justice)
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SchemeExists ThwartableScheme
    }

-- Ant-Man

giantStomp :: CardDef
giantStomp =
  (identityEvent "12003" "Giant Stomp" 3 HeroAction [Attack, Giant] [Energy])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = SelfMatches (IdentityWithTrait Giant) <> canAttack
    }

hiveMind :: CardDef
hiveMind =
  (identityEvent "12004" "Hive Mind" 2 HeroAction [Thwart, Tiny] [Physical])
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SelfMatches (IdentityWithTrait Tiny) <> SchemeExists ThwartableScheme
    }

resize :: CardDef
resize = identityEvent "12005" "Resize" 0 HeroAction [Superpower] [Physical]

callForAid :: CardDef
callForAid = event "12015" "Call for Aid" 0 HeroAction [] [Physical] Leadership

moxie :: CardDef
moxie =
  reprintedAs ["21017"] $ (event "12016" "Moxie" 0 HeroResponse [] [Mental] Leadership)
    { cdResponseWindow = Just (IdentityChangedAnyForm After You)
    }

-- | Team-Up (Ant-Man and Wasp): played only while both are in play, as hero or ally
-- (Ant-Man rulesheet).
swarmTactics :: CardDef
swarmTactics = deckLimited 1 $ teamUpNames "Ant-Man" "Wasp" $ reprintedAs ["13020"] $
  (basicEvent "12020" "Swarm Tactics" 1 HeroAction [Tactic] [Mental])
    { cdCriteria = teamUp "Ant-Man" "Wasp"
    }

teamUp :: Text -> Text -> Criteria
teamUp a b = inPlay a <> inPlay b
 where
  -- an ally counts by its title or its subtitle (RR Team-Up)
  inPlay t = AnyCriteria [CharacterExists (IdentityCharacters $ IdentityWithTitle t <> HeroIdentity), AllyExists (AllyNamedAs t)]

momentOfTriumph :: CardDef
momentOfTriumph = reprintedAs ["43017"] $
  (event "12030" "Moment of Triumph" 0 HeroResponse [] [Energy] Aggression)
    { cdResponseWindow = Just (EnemyDefeated After AnyEnemy (AttackFromPlayer You))
    }

layDownTheLaw :: CardDef
layDownTheLaw =
  (event "12031" "Lay Down the Law" 1 HeroResponse [Thwart] [Mental] Justice)
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdResponseWindow = Just (IdentityChangedAnyForm After You)
    , cdCriteria = SchemeExists ThwartableScheme
    }

musterCourage :: CardDef
musterCourage =
  (event "12032" "Muster Courage" 3 HeroAction [] [Physical] Protection)
    { cdCriteria = SelfMatches (IdentityWithTrait Avenger)
    }

assessTheSituation :: CardDef
assessTheSituation = basicEvent "12033" "Assess the Situation" 0 Action [Tactic] [Mental]

-- Wasp

giantHelp :: CardDef
giantHelp =
  (identityEvent "13003" "Giant Help" 2 HeroAction [Thwart] [Mental])
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SchemeExists ThwartableScheme
    }

pinpointStrike :: CardDef
pinpointStrike =
  (identityEvent "13004" "Pinpoint Strike" 3 HeroAction [Attack] [Mental])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

-- | "When you use one of your hero's basic powers (THW, ATK, or DEF)".
rapidGrowth :: CardDef
rapidGrowth =
  (identityEvent "13005" "Rapid Growth" 1 HeroInterrupt [Giant, Superpower] [Energy])
    { cdResponseWindow = Just (WindowOneOf [MakesBasicThwart When You, MakesBasicAttack When You, HeroDefended When You AnyEnemy])
    }

-- | Two Hero Actions, one for each hero form: played in either.
waspSting :: CardDef
waspSting =
  (identityEvent "13006" "Wasp Sting" 2 HeroAction [Attack] [Mental])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = AnyCriteria [SelfMatches (IdentityWithTrait Giant), SelfMatches (IdentityWithTrait Tiny)] <> canAttack
    }

intoTheFray :: CardDef
intoTheFray = reprintedAs ["16042"] $
  (event "13013" "Into the Fray" 3 HeroAction [Attack] [Energy] Aggression)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttackMinion
    }

surpriseAttack :: CardDef
surpriseAttack =
  (event "13014" "Surprise Attack" 1 HeroResponse [Attack] [Physical] Aggression)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdResponseWindow = Just (IdentityChangedAnyForm After You)
    , cdCriteria = canAttack
    }

athleticConditioning :: CardDef
athleticConditioning = reprintedAs ["19021"] $ basicEvent "13034" "Athletic Conditioning" 1 HeroAction [] [Mental]

runningInterference :: CardDef
runningInterference =
  (event "13031" "Running Interference" 2 HeroAction [Thwart] [Physical] Justice)
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SelfMatches (IdentityWithTrait Avenger)
    }

allForOne :: CardDef
allForOne =
  (event "13032" "All for One" 2 HeroAction [Attack] [Energy] Leadership)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

perseverance :: CardDef
perseverance = reprintedAs ["32016"] $
  (event "13033" "Perseverance" 1 HeroResponse [Tactic] [Mental] Protection)
    { cdResponseWindow = Just (IdentityChangedAnyForm After You)
    }

-- Quicksilver

alwaysBeRunning :: CardDef
-- | "Ready Quicksilver": only while he is exhausted (RR Target)
alwaysBeRunning = (identityEvent "14003" "Always Be Running" 1 HeroAction [Superpower] [Energy]) {cdCriteria = Exhausted}

doubleTime :: CardDef
doubleTime = identityEvent "14004" "Double Time" 2 HeroAction [Superpower] [Mental]

maximumVelocity :: CardDef
maximumVelocity =
  (identityEvent "14005" "Maximum Velocity" 2 HeroAction [Superpower] [Physical])
    { cdLimit = Just (MaxPerPhase 1)
    }

-- | Its cost is X.
speedCyclone :: CardDef
speedCyclone =
  (identityEvent "14006" "Speed Cyclone" 0 HeroAction [Superpower] [Energy])
    { cdAbilityCost = DynamicResourceCost Nothing
    }

neverBackDown :: CardDef
neverBackDown =
  (event "14014" "Never Back Down" 1 HeroInterrupt [Defense] [Physical] Protection)
    { cdResponseWindow = Just (HeroDefended When You AnyEnemy)
    , cdAbilitySubTypes = [Ability.Defense]
    }

sideStep :: CardDef
sideStep =
  deckLimited 3 $ reprintedAs ["26019"] $ (event "14015" "Side Step" 1 HeroInterrupt [Defense] [Mental] Protection)
    { cdResponseWindow = Just (WouldTakeDamage You FromAnyDamageSource AnyValue)
    , cdAbilitySubTypes = [Ability.Defense]
    }

-- | Team-Up (Quicksilver and Scarlet Witch). Max 1 per deck.
orderAndChaos :: CardDef
orderAndChaos = deckLimited 1 $ teamUpNames "Quicksilver" "Scarlet Witch" $ reprintedAs ["15018"] $
  (basicEvent "14018" "Order and Chaos" 1 HeroInterrupt [] [Physical])
    { cdCriteria = teamUp "Quicksilver" "Scarlet Witch"
    , cdResponseWindow = Just (TreacheryRevealed When AnyTreachery RevealedFromEncounterDeck)
    }

unitedWeStand :: CardDef
unitedWeStand =
  (event "14031" "United We Stand" 0 HeroAction [] [Physical] Leadership)
    { cdCriteria = SelfMatches (IdentityWithTrait Avenger)
    }

beatEmUp :: CardDef
beatEmUp = basicEvent "14032" "Beat 'Em Up" 2 HeroAction [Tactic] [Energy]

-- Scarlet Witch

chaosMagic :: CardDef
chaosMagic =
  (identityEvent "15003" "Chaos Magic" 0 HeroAction [Spell, Superpower] [Wild])
    { -- a card to play (RR Target); not Chaos Magic itself, still in hand as it is checked
      cdCriteria =
        ExtendedCardExists $
          InHandOf You $
            PlayableIgnoringCostBy You $
              BasicCardMatches (NotCardMatching $ CardWithTitle "Chaos Magic")
    }

hexBolt :: CardDef
hexBolt = identityEvent "15004" "Hex Bolt" 2 HeroAction [Spell, Superpower] [Mental]

molecularDecay :: CardDef
molecularDecay =
  (identityEvent "15005" "Molecular Decay" 3 HeroAction [Attack, Spell] [Energy])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

warpReality :: CardDef
warpReality =
  (identityEvent "15006" "Warp Reality" 1 HeroInterrupt [Spell, Superpower] [Physical])
    { cdResponseWindow = Just (EncounterCardRevealedFromDeck When AnyEncounterCard)
    , cdCriteria = CanCancelRevealedEncounterCard
    }

crisisAverted :: CardDef
crisisAverted =
  (event "15012" "Crisis Averted" 3 HeroAction [Thwart] [Mental] Justice)
    { cdAbilitySubTypes = [Ability.Thwart]
    }

multitasking :: CardDef
multitasking =
  (event "15013" "Multitasking" 1 HeroAction [Thwart] [Physical] Justice)
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SchemeExists ThwartableScheme
    }

swiftRetribution :: CardDef
swiftRetribution = reprintedAs ["34019"] $
  (event "15014" "Swift Retribution" 1 HeroAction [Attack] [Mental] Justice)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttackVillain
    }

turnTheTide :: CardDef
turnTheTide =
  (event "15015" "Turn the Tide" 0 HeroResponse [Attack] [Energy] Justice)
    { cdResponseWindow = Just (HeroThwartsAllThreat You)
    , cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

spiritualMeditation :: CardDef
spiritualMeditation = reprintedAs ["45052"] $
  (basicEvent "15019" "Spiritual Meditation" 0 Action [Spell] [Mental])
    { cdCriteria = SelfMatches (IdentityWithTrait Mystic)
    }

browbeat :: CardDef
browbeat =
  (event "15028" "Browbeat" 2 HeroAction [Attack] [Physical] Aggression)
    { cdCriteria = SelfMatches (IdentityWithTrait Avenger) <> canAttackVillain
    , cdAbilitySubTypes = [Ability.Attack]
    }

lastStand :: CardDef
lastStand =
  (event "15029" "Last Stand" 0 HeroInterrupt [Tactic] [Mental] Leadership)
    { cdResponseWindow = Just (AllyAttacked When (AllyControlledBy You) AnyEnemy)
    }

baitAndSwitch :: CardDef
baitAndSwitch = reprintedAs ["32015", "27013"] $
  (event "15030" "Bait and Switch" 1 HeroAction [Thwart] [Energy] Protection)
    { cdAbilitySubTypes = [Ability.Thwart]
    }

recuperation :: CardDef
recuperation =
  reprintedAs ["46022"] ((basicEvent "15031" "Recuperation" 2 AlterEgoAction [] [Physical])
    { cdCriteria = SelfMatches IdentityWithAnyDamage
    })

-- Star-Lord

daringEscape :: CardDef
daringEscape =
  (identityEvent "17003" "Daring Escape" 0 HeroAction [] [Energy])
    { cdAbilityCost = DealEncounterCardCost
    }

gutsyMove :: CardDef
gutsyMove =
  (identityEvent "17004" "Gutsy Move" 2 HeroAction [Thwart] [Mental])
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SchemeExists ThwartableScheme
    }

slidingShot :: CardDef
slidingShot =
  (identityEvent "17005" "Sliding Shot" 3 HeroAction [Attack] [Physical])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = UpgradeExists (UpgradeIs Upgrades.elementGun <> UpgradeControlledBy You) <> canAttack
    }

airSupremacy :: CardDef
airSupremacy = event "17014" "Air Supremacy" 2 HeroAction [Aerial, Tactic] [Physical] Leadership

blazeOfGlory :: CardDef
blazeOfGlory =
  (event "17015" "Blaze of Glory" 2 HeroAction [] [Physical] Leadership)
    { cdLimit = Just (MaxPerRound 1)
    }

diveBomb :: CardDef
diveBomb =
  (event "17028" "Dive Bomb" 4 HeroAction [Aerial, Attack] [Physical] Aggression)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = SelfMatches (IdentityWithTrait Aerial) <> canAttack
    }

agileFlight :: CardDef
agileFlight =
  (event "17029" "Agile Flight" 3 HeroAction [Aerial, Thwart] [Mental] Justice)
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SelfMatches (IdentityWithTrait Aerial) <> SchemeExists ThwartableScheme
    }

everVigilant :: CardDef
everVigilant = reprintedAs ["42015"] $
  (event "17030" "Ever Vigilant" 2 HeroAction [Aerial, Skill] [Mental] Protection)
    { cdCriteria = SelfMatches (IdentityWithTrait Aerial)
    }

-- Gamora

acrobaticMove :: CardDef
acrobaticMove =
  (identityEvent "18003" "Acrobatic Move" 0 HeroAction [Attack] [Energy])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

crosscounter :: CardDef
crosscounter =
  (identityEvent "18004" "Crosscounter" 1 HeroInterrupt [Attack, Defense, Thwart] [Physical])
    { cdAbilitySubTypes = [Ability.Attack, Ability.Defense, Ability.Thwart]
    , cdResponseWindow = Just (WouldTakeDamage You FromAnyDamageSource AnyValue)
    }

setThePace :: CardDef
setThePace =
  (identityEvent "18005" "Set the Pace" 0 HeroAction [Thwart] [Physical])
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SchemeExists ThwartableScheme
    }

decisiveBlow :: CardDef
decisiveBlow =
  (identityEvent "18006" "Decisive Blow" 2 HeroAction [Attack] [Physical])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

forwardMomentum :: CardDef
forwardMomentum =
  (identityEvent "18007" "Forward Momentum" 2 HeroAction [Thwart] [Mental])
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SchemeExists ThwartableScheme
    }

clobber :: CardDef
clobber = reprintedAs ["45046"] $
  (event "18012" "Clobber" 2 HeroAction [Attack] [Physical] Aggression)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

planOfAttack :: CardDef
planOfAttack = event "18013" "Plan of Attack" 0 Action [Tactic] [Mental] Aggression

-- | A Hero Interrupt played when a minion initiates an attack, and a Hero Action played
-- against the villain.
firstHit :: CardDef
firstHit =
  (event "18015" "First Hit" 1 HeroInterrupt [Attack] [Energy] Protection)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdResponseWindow = Just (EnemyWouldAttack MinionEnemy AnyIdentity)
    , cdActionCriteria = Just canAttackVillain
    }

impede :: CardDef
impede =
  (event "18016" "Impede" 2 HeroAction [Thwart] [Mental] Justice)
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SchemeExists UnpatrolledMainScheme
    }

hitAndRun :: CardDef
hitAndRun = reprintedAs ["37020"] $
  (basicEvent "18020" "Hit and Run" 3 HeroAction [Attack, Thwart] [Energy])
    { cdAbilitySubTypes = [Ability.Attack, Ability.Thwart]
    , cdCriteria = canAttack
    }

pivotalMoment :: CardDef
pivotalMoment =
  (event "18029" "Pivotal Moment" 2 HeroAction [Attack] [Physical] Justice)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttackVillain
    }

trueGrit :: CardDef
trueGrit =
  (event "18031" "True Grit" 1 Response [Thwart] [Energy] Protection)
    { cdResponseWindow = Just (HeroDefended After You AnyEnemy)
    , cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SchemeExists ThwartableScheme
    }

-- Groot

fruition :: CardDef
fruition = identityEvent "16002" "Fruition" 0 Action [Superpower] [Mental]

iAmGroot :: CardDef
iAmGroot =
  (identityEvent "16003" "\"I am Groot\"" 3 HeroAction [Superpower, Thwart] [Mental])
-- Drax

fightMeCoward :: CardDef
fightMeCoward = identityEvent "19003" "\"Fight Me, Coward!\"" 0 HeroAction [] [Physical]

intimidation :: CardDef
intimidation =
  (identityEvent "19004" "Intimidation" 1 HeroAction [Thwart] [Mental])
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SchemeExists ThwartableScheme
    }

iAmGrootExclaim :: CardDef
iAmGrootExclaim =
  (identityEvent "16004" "\"I. AM. GROOT!\"" 2 HeroAction [Attack, Superpower] [Physical])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

rootStomp :: CardDef
rootStomp =
  (identityEvent "16005" "Root Stomp" 2 HeroAction [Attack, Superpower] [Physical])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

weAreGroot :: CardDef
weAreGroot = identityEvent "16006" "\"We Are Groot\"" 1 HeroAction [Superpower] [Mental]

fightingFit :: CardDef
fightingFit =
  (event "16014" "Fighting Fit" 2 HeroAction [Attack] [Physical] Protection)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttackVillain
    }

-- | Team-Up (Groot and Rocket Raccoon). Max 1 per deck.
floraAndFauna :: CardDef
floraAndFauna = deckLimited 1 $ teamUpNames "Groot" "Rocket Raccoon" $ reprintedAs ["16048"] $
  (basicEvent "16020" "Flora and Fauna" 1 HeroAction [] [Wild])
    { cdCriteria = teamUp "Groot" "Rocket Raccoon"
    }

-- Rocket Raccoon

-- | "After you make a basic thwart (using your THW)".
iveGotAPlan :: CardDef
iveGotAPlan =
  (identityEvent "16030" "I've Got a Plan" 1 HeroResponse [Tactic] [Mental])
    { cdResponseWindow = Just (MakesBasicThwart After You)
    }

reload :: CardDef
reload = identityEvent "16031" "Reload" 1 HeroAction [Skill] [Energy]

schadenfreude :: CardDef
schadenfreude = identityEvent "16032" "Schadenfreude" 2 HeroAction [] [Physical]

-- | Its cost is discarding encounter cards until a minion, put into play engaged with
-- you (Defender of the Nine Realms').
lookingForTrouble :: CardDef
lookingForTrouble =
  reprintedAs ["46017"] ((event "16043" "Looking for Trouble" 0 HeroAction [Thwart] [Physical] Aggression)
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdAbilityCost = DiscardEncounterCardsUntilMinionCost
    })

-- The Galaxy's Most Wanted: The Market (unit costs in Marvel.Campaign.marketUnitCosts)

brainstorm :: CardDef
brainstorm =
  (marketEvent "16150" "Brainstorm" 0 HeroAction [Thwart])
    { cdAbilitySubTypes = [Ability.Thwart]
    }

byAnyMeans :: CardDef
byAnyMeans =
  (marketEvent "16151" "By Any Means" 0 HeroAction [Attack])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttackVillain
    }

contingencyPlan :: CardDef
contingencyPlan =
  (marketEvent "16152" "Contingency Plan" 0 HeroAction [Attack])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

inDefiance :: CardDef
inDefiance =
  (marketEvent "16153" "In Defiance" 0 HeroInterrupt [])
    { cdResponseWindow = Just (WouldTakeDamage AnyIdentity FromAttack AnyValue)
    }

calculateTheOdds :: CardDef
calculateTheOdds = marketEvent "16154" "Calculate the Odds" 0 HeroAction []

creativeSolution :: CardDef
creativeSolution = marketEvent "16155" "Creative Solution" 0 HeroAction []

grapple :: CardDef
grapple = marketEvent "16156" "Grapple" 0 HeroAction []

wingIt :: CardDef
wingIt = marketEvent "16157" "Wing It" 0 HeroAction []

closeCall :: CardDef
closeCall =
  (marketEvent "16158" "Close Call" 0 HeroInterrupt [])
    { cdResponseWindow = Just (BoostCardFlipped When AnyEnemy)
    }

defyDanger :: CardDef
defyDanger =
  (marketEvent "16159" "Defy Danger" 1 HeroAction [Attack])
knifeLeap :: CardDef
knifeLeap =
  (identityEvent "19005" "Knife Leap" 3 HeroInterrupt [Skill] [Physical])
    { cdResponseWindow = Just (MakesBasicAttack When You)
    }

parry :: CardDef
parry =
  (identityEvent "19006" "Parry" 0 HeroInterrupt [Defense] [Energy])
    { cdAbilitySubTypes = [Ability.Defense]
    , cdResponseWindow = Just (WouldTakeDamage You FromAnyDamageSource AnyValue)
    }

payback :: CardDef
payback =
  (identityEvent "19007" "Payback" 0 HeroResponse [Attack] [Energy])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdResponseWindow = Just (EnemyAttacked After VillainEnemy You)
    }

deflection :: CardDef
deflection =
  (event "19015" "Deflection" 2 HeroInterrupt [Skill] [Mental] Protection)
    { cdResponseWindow = Just (WouldTakeDamage AnyIdentity FromAttack AnyValue)
    }

hardKnocks :: CardDef
hardKnocks =
  (event "19016" "Hard Knocks" 3 HeroAction [Attack] [Physical] Protection)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

inHarmsWay :: CardDef
inHarmsWay =
  (marketEvent "16160" "In Harm's Way" 1 HeroAction [Thwart])
leadingBlow :: CardDef
leadingBlow =
  (event "19017" "Leading Blow" 0 HeroInterrupt [Tactic] [Energy] Protection)
    { cdResponseWindow = Just (MakesBasicAttack When You)
    , cdAbilityCost = DiscardTopEncounterCardCost
    }

subdue :: CardDef
subdue =
  (event "19018" "Subdue" 1 HeroInterrupt [] [Energy] Protection)
    { cdResponseWindow = Just (EnemyAttacked When AnyEnemy AnyIdentity)
    }

bringIt :: CardDef
bringIt =
  (event "19030" "\"Bring It!\"" 0 HeroAction [] [Energy] Aggression)
    { cdLimit = Just (MaxPerPhase 1)
    }

thinkFast :: CardDef
thinkFast =
  (event "19031" "\"Think Fast!\"" 1 HeroAction [] [Energy] Justice)
    { cdCriteria = SelfMatches (IdentityWithTrait Guardian)
    }

-- Venom

behindEnemyLines :: CardDef
behindEnemyLines =
  (identityEvent "20002" "Behind Enemy Lines" 2 HeroAction [Thwart] [Mental])
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SchemeExists ThwartableScheme
    }

takeTheFightToThem :: CardDef
takeTheFightToThem = marketEvent "16161" "Take the Fight to Them" 0 HeroAction []

ardentResolve :: CardDef
ardentResolve = marketEvent "16166" "Ardent Resolve" 0 HeroAction []

onrush :: CardDef
onrush =
  (marketEvent "16167" "Onrush" 0 HeroInterrupt [])
    { cdResponseWindow = Just (EncounterCardRevealedFromDeck When AnyEncounterCard)
    , cdCriteria = CanCancelRevealedEncounterCard
    }

safeguard :: CardDef
safeguard = marketEvent "16168" "Safeguard" 0 HeroAction []

sureGamble :: CardDef
sureGamble = marketEvent "16169" "Sure Gamble" 0 HeroAction []

grandStrategy :: CardDef
grandStrategy = marketEvent "16174" "Grand Strategy" 0 HeroAction []

powerUnleashed :: CardDef
powerUnleashed = marketEvent "16175" "Power Unleashed" 0 HeroAction []

triedAndTrue :: CardDef
triedAndTrue = marketEvent "16176" "Tried and True" 0 HeroAction []

tripleThreat :: CardDef
tripleThreat =
  (marketEvent "16177" "Triple Threat" 0 HeroAction [])
    { -- readying is its only effect: an exhausted character to ready (RR Target); enemies
      -- never exhaust, so only friendly ones can be
      cdCriteria =
        AnyCriteria
          [ CharacterExists (IdentityCharacters ExhaustedIdentity)
          , AllyExists ExhaustedAlly
          ]
    }

-- | A card of The Market: of no aspect and not basic, bought with units in The Galaxy's
-- Most Wanted campaign (rulebook p.5); its deckbuilding classification is its set.
marketEvent :: CardCode -> Name -> Int -> AbilityType -> [Trait] -> CardDef
marketEvent code name cost aType traits =
  (baseEvent code name cost aType traits [Wild] Nothing) {cdEncounterSet = Just TheMarket}
graspingTendrils :: CardDef
graspingTendrils =
  (identityEvent "20003" "Grasping Tendrils" 2 HeroInterrupt [Defense] [Physical])
    { cdAbilitySubTypes = [Ability.Defense]
    , cdResponseWindow = Just (EnemyWouldAttack VillainEnemy You)
    }

lockedAndLoaded :: CardDef
lockedAndLoaded = identityEvent "20004" "Locked and Loaded" 0 Action [] [Mental]

runAndGun :: CardDef
runAndGun = identityEvent "20005" "Run and Gun" 3 HeroAction [Skill] [Energy]

savageAttack :: CardDef
savageAttack =
  (identityEvent "20006" "Savage Attack" 2 HeroAction [Attack] [Physical])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

scareTactic :: CardDef
scareTactic =
  (event "20012" "Scare Tactic" 1 HeroAction [Attack] [Mental] Justice)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = AnyCriteria [SelfMatches StunnedIdentity, EnemyExists (AttackableEnemy <> ConfusedEnemy)]
    }

makingAnEntrance :: CardDef
makingAnEntrance = reprintedAs ["30016"] $
  (event "20013" "Making an Entrance" 1 HeroInterrupt [Skill] [Physical] Justice)
    { cdResponseWindow = Just (MakesBasicThwart When You)
    }

fusillade :: CardDef
fusillade =
  (event "20026" "Fusillade" 2 HeroAction [Attack] [Mental] Aggression)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    , cdAbilityCost = ExhaustUpgradeCost (UpgradeWithTrait Weapon)
    }

welcomeAboard :: CardDef
welcomeAboard =
  (event "20027" "\"Welcome Aboard\"" 0 HeroAction [] [Energy] Leadership)
    { cdCriteria = SelfMatches (IdentityWithTrait Guardian)
    , cdLimit = Just (MaxPerRound 1)
    }

shakeItOff :: CardDef
shakeItOff =
  (event "20028" "Shake it Off" 1 HeroResponse [Skill] [Physical] Protection)
    { cdResponseWindow = Just (EnemyAttackedAndDamaged AnyEnemy (CharacterWithTrait Guardian))
    }

-- Spectrum

gammaBlast :: CardDef
gammaBlast =
  (identityEvent "21007" "Gamma Blast" 3 HeroAction [Attack, Superpower] [Physical])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

photonSpeed :: CardDef
photonSpeed =
  (identityEvent "21008" "Photon Speed" 2 HeroAction [Thwart, Superpower] [Mental])
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SchemeExists ThwartableSchemeIgnoringCrisis
    }

pulsarShield :: CardDef
pulsarShield =
  (identityEvent "21009" "Pulsar Shield" 1 HeroInterrupt [Defense, Superpower] [Energy])
    { cdResponseWindow = Just (HeroDefended When You AnyEnemy)
    , cdAbilitySubTypes = [Ability.Defense]
    }

speedOfLight :: CardDef
speedOfLight = identityEvent "21010" "Speed of Light" 0 HeroAction [Superpower] [Wild]

-- | "Exhaust 3 allies you control that share a Trait with your hero": each shares one
-- (R12).
massAttack :: CardDef
massAttack =
  (event "21016" "Mass Attack" 3 HeroAction [Attack] [Physical] Leadership)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    , cdAbilityCost = ExhaustAlliesMatchingCost AllySharingTraitWithController 3
    }

-- Adam Warlock

-- | "Discard up to 4 cards from the top of your deck" is part of its cost.
karmicBlast :: CardDef
karmicBlast =
  (identityEvent "21038" "Karmic Blast" 2 HeroAction [Attack, Superpower] [Physical])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    , cdAbilityCost = DiscardTopOfDeckCardsCost 0 4
    }

cosmicAwareness :: CardDef
cosmicAwareness =
  (identityEvent "21039" "Cosmic Awareness" 2 HeroAction [Thwart, Superpower] [Mental])
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SchemeExists ThwartableScheme
    , cdAbilityCost = DiscardTopOfDeckCardsCost 0 4
    }

-- | Its target, a card in your discard pile, must be there (RR Target).
quantumMagic :: CardDef
quantumMagic =
  (identityEvent "21040" "Quantum Magic" 1 Action [Spell] [Energy])
    { cdCriteria = ExtendedCardExists (InDiscardOf You (BasicCardMatches AnyCard))
    }

-- | A Cosmic Entity: shuffled into the encounter deck when played, its "When Revealed"
-- resolved by whoever reveals it.
cosmicEntity :: CardCode -> Name -> Resource -> Aspect -> CardDef
cosmicEntity code name r aspect = (event code name 2 Action [CosmicEntity] [r] aspect) {cdUnique = True}

inBetweener :: CardDef
inBetweener = cosmicEntity "21042" "In-Betweener" Mental Aggression

-- | Max 1 per deck.
magicAttack :: CardDef
magicAttack =
  (event "21043" "Magic Attack" 1 HeroAction [Attack, Spell] [Physical] Aggression)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack <> SelfMatches (IdentityWithTrait Mystic)
    , cdAbilityCost = DiscardTopOfDeckCardsCost 0 5
    }

livingTribunal :: CardDef
livingTribunal = cosmicEntity "21048" "Living Tribunal" Mental Justice

-- | Max 1 per deck.
zoneOfSilence :: CardDef
zoneOfSilence =
  (event "21050" "Zone of Silence" 1 HeroAction [Spell, Thwart] [Mental] Justice)
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = SchemeExists ThwartableScheme <> SelfMatches (IdentityWithTrait Mystic)
    , cdAbilityCost = DiscardTopOfDeckCardsCost 0 4
    }

eternity :: CardDef
eternity = cosmicEntity "21054" "Eternity" Energy Leadership

-- | Max 1 per deck.
summoningSpell :: CardDef
summoningSpell =
  (event "21055" "Summoning Spell" 2 HeroAction [Spell] [Mental] Leadership)
    { cdCriteria = SelfMatches (IdentityWithTrait Mystic)
    }

theGardener :: CardDef
theGardener = cosmicEntity "21060" "The Gardener" Physical Protection

-- | Max 1 per deck. Its cost is the deck's top cards, as many as the damage: a deck with
-- fewer cannot pay it (R17).
shieldSpell :: CardDef
shieldSpell =
  (event "21061" "Shield Spell" 0 HeroInterrupt [Defense, Spell] [Energy] Protection)
    { cdResponseWindow = Just (WouldTakeDamage You FromAttack AnyValue)
    , cdAbilitySubTypes = [Ability.Defense]
    , cdCriteria = SelfMatches (IdentityWithTrait Mystic)
    , cdAbilityCost = DiscardTopOfDeckForDamageCost
    }

-- Nebula

combatReady :: CardDef
combatReady = deckLimited 2 $ identityEvent "22009" "Combat Ready" 0 AlterEgoAction [Skill] [Physical]

lethalIntent :: CardDef
lethalIntent = deckLimited 3 $ (identityEvent "22010" "Lethal Intent" (-1) HeroAction [] [Mental])
    { cdAbilityCost = PrintedXCost 1 (UpgradeWithTrait Technique <> UpgradeControlledBy You <> UpgradeWithResolvableSpecial)
    }

oneWayOrAnother :: CardDef
oneWayOrAnother = reprintedAs ["30017"] $ deckLimited 3 $ (event "22015" "One Way or Another" 0 HeroAction [] [Mental] Justice)
    { cdLimit = Just (MaxPerRound 1)
    , cdAbilityCost = SearchEncounterSideSchemeCost
    }

brainsOverBrawn :: CardDef
brainsOverBrawn = deckLimited 3 $ (event "22018" "Brains Over Brawn" 2 HeroResponse [Attack] [Physical] Justice)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    , cdResponseWindow = Just (MakesBasicThwart After You)
    }

daughtersOfThanos :: CardDef
daughtersOfThanos = deckLimited 1 $ teamUpNames "Gamora" "Nebula" $ (basicEvent "22022" "Daughters of Thanos" 1 HeroAction [] [Wild])
    { cdCriteria = teamUp "Gamora" "Nebula"
    }

-- War Machine

repulsorBeam :: CardDef
repulsorBeam = deckLimited 2 $ (identityEvent "23008" "Repulsor Beam" 1 HeroAction [Attack] [Energy])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    , cdAbilityCost = IdentityCountersCost 1
    }

targetedStrike :: CardDef
targetedStrike = deckLimited 2 $ (identityEvent "23009" "Targeted Strike" 1 HeroAction [Thwart] [Energy])
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists SchemeYouCanThwart]
    , cdAbilityCost = IdentityCountersCost 1
    }

scorchedEarth :: CardDef
scorchedEarth = deckLimited 2 $ (identityEvent "23010" "Scorched Earth" 3 HeroAction [Tactic] [Mental])
    { cdAbilityCost = IdentityCountersCost 3
    }

fullAuto :: CardDef
fullAuto = deckLimited 2 $ (identityEvent "23011" "Full Auto" 2 HeroAction [Attack] [Physical])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    , cdAbilityCost = IdentityCountersCost 4
    }

sneakAttack :: CardDef
sneakAttack = deckLimited 3 $ (event "23017" "Sneak Attack" 1 Action [Tactic] [Physical] Leadership)
    { cdAbilityCost = ChosenHandCardCost
    }

saveTheDay :: CardDef
saveTheDay = deckLimited 3 $ (event "23018" "Save the Day" 1 HeroAction [Tactic] [Mental] Leadership)
    { cdAbilityCost = DiscardAllyCost (AllyWithPrintedCostAtLeast 1)
    , cdCriteria = SchemeExists ThwartableScheme
    }

goDownSwinging :: CardDef
goDownSwinging = deckLimited 3 $ (event "23019" "Go Down Swinging" 0 HeroAction [Tactic] [Physical] Leadership)
    { cdAbilityCost = DiscardAllyCost (AllyWithPrintedCostAtLeast 1)
    }

twoAgainstTheWorld :: CardDef
twoAgainstTheWorld = deckLimited 1 $ teamUpNames "Iron Man" "War Machine" $ (basicEvent "23024" "Two Against the World" 3 HeroAction [] [Energy])
    { cdCriteria = teamUp "Iron Man" "War Machine"
    }

asOne :: CardDef
asOne = deckLimited 3 $ (event "23032" "As One!" 2 HeroAction [Attack] [Physical] Aggression)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    , cdAbilityCost = ExhaustCharacterWithTraitCost Avenger <> ExhaustCharacterWithTraitCost Guardian
    , cdKeywords = fromList [Alliance]
    }

standTogether :: CardDef
standTogether = deckLimited 3 $ (event "23034" "Stand Together" 4 HeroInterrupt [Tactic] [Energy] Protection)
    { cdAbilityCost = ExhaustCharacterWithTraitCost Avenger <> ExhaustCharacterWithTraitCost Guardian
    , cdKeywords = fromList [Alliance]
    , cdResponseWindow = Just (CharacterWouldTakeAttackDamage (CharacterControlledBy AnyIdentity))
    }

-- Valkyrie

visitValhalla :: CardDef
visitValhalla = deckLimited 1 $ (identityEvent "25009" "Visit Valhalla" 0 AlterEgoAction [] [Physical])
    { cdCriteria = ExtendedCardExists (InDiscardOf You (BasicCardMatches (HeroSpecificCard <> CardMatchesAny (map CardWithTitle ["Death-Glow", "Annabelle Riggs", "Valhalla", "Valkyrie's Spear", "Dragonfang", "Aragorn", "Flight of the Valkyrior", "Visit Valhalla", "Chooser of the Slain", "Shieldmaiden", "Have at Thee!"]))))
    }

chooserOfTheSlain :: CardDef
chooserOfTheSlain = deckLimited 2 $ (identityEvent "25010" "Chooser of the Slain" 0 HeroAction [] [Mental])
    { cdAbilityCost = SearchEncounterMinionCost
    }

shieldmaiden :: CardDef
shieldmaiden = deckLimited 2 $ (identityEvent "25011" "Shieldmaiden" 1 HeroInterrupt [Defense] [Physical])
    { cdAbilitySubTypes = [Ability.Defense]
    , cdResponseWindow = Just (EnemyAttacked When (EnemyWithUpgrade (UpgradeIs Upgrades.deathGlow)) AnyIdentity)
    }

haveAtThee :: CardDef
haveAtThee = deckLimited 3 $ (identityEvent "25012" "Have at Thee!" 3 HeroAction [Attack] [Energy])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

quickStrike :: CardDef
quickStrike = deckLimited 3 $ (event "25018" "Quick Strike" 2 HeroAction [Attack] [Physical] Aggression)
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

smashTheProblem :: CardDef
smashTheProblem = deckLimited 3 $ (event "25019" "Smash the Problem" 1 HeroAction [Thwart] [Mental] Aggression)
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists SchemeYouCanThwart]
    , cdAbilityCost = ExhaustCost
    }

theBestDefense :: CardDef
theBestDefense = deckLimited 3 $ (event "25020" "The Best Defense…" 0 HeroInterrupt [Defense] [Physical] Aggression)
    { cdAbilitySubTypes = [Ability.Defense]
    , cdResponseWindow = Just (HeroDefended When You AnyEnemy)
    }

godlikeStamina :: CardDef
godlikeStamina = deckLimited 3 $ (basicEvent "25024" "Godlike Stamina" 0 Action [] [Physical])
    { cdCriteria = SelfMatches (IdentityWithTrait Asgard)
    }

problemSolvers :: CardDef
problemSolvers = deckLimited 3 $ (event "25033" "Problem Solvers" 4 HeroAction [Thwart] [Physical] Justice)
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists SchemeYouCanThwart]
    , cdAbilityCost = ExhaustCharacterWithTraitCost Avenger <> ExhaustCharacterWithTraitCost Guardian
    , cdKeywords = fromList [Alliance]
    }

cosmicAlliance :: CardDef
cosmicAlliance = deckLimited 3 $ (basicEvent "25036" "Cosmic Alliance" 3 HeroAction [] [Energy])
    { cdKeywords = fromList [Alliance]
    , cdAbilityCost = ChooseCharacterWithTraitCost Avenger CharacterCanBeReadied <> ChooseCharacterWithTraitCost Guardian CharacterCanBeReadied
    }

-- Vision

solarBeam :: CardDef
solarBeam = deckLimited 3 $ (identityEvent "26008" "Solar Beam" 3 HeroAction [Attack, Superpower, Thwart] [Energy])
    { cdCriteria = AnyCriteria [UpgradeExists (UpgradeIs Upgrades.dense <> UpgradeControlledBy You) <> canAttack, UpgradeExists (UpgradeIs Upgrades.intangible <> UpgradeControlledBy You) <> AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists SchemeYouCanThwart]]
    }

superdenseStrike :: CardDef
superdenseStrike = deckLimited 2 $ (identityEvent "26009" "Superdense Strike" 2 HeroAction [Attack, Superpower] [Physical])
    { cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = UpgradeExists (UpgradeIs Upgrades.dense <> UpgradeControlledBy You) <> canAttack
    }

justPassingThrough :: CardDef
justPassingThrough = deckLimited 2 $ (identityEvent "26010" "Just Passing Through" 1 HeroAction [Superpower, Thwart] [Mental])
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = UpgradeExists (UpgradeIs Upgrades.intangible <> UpgradeControlledBy You) <> AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists (ThwartableSchemeIgnoringCrisis <> SchemeWithoutThwartPrevention)]
    }

phaseDisruption :: CardDef
phaseDisruption = deckLimited 1 $ (identityEvent "26011" "Phase Disruption" 2 HeroAction [Superpower] [Energy])
    { cdCriteria = UpgradeExists (UpgradeIs Upgrades.intangible <> UpgradeControlledBy You)
    }

massIncrease :: CardDef
massIncrease = deckLimited 1 $ (identityEvent "26012" "Mass Increase" 1 HeroInterrupt [Defense, Superpower] [Physical])
    { cdAbilitySubTypes = [Ability.Defense]
    , cdResponseWindow = Just (HeroDefended When You AnyEnemy)
    , cdCriteria = UpgradeExists (UpgradeIs Upgrades.dense <> UpgradeControlledBy You)
    }

defiance :: CardDef
defiance = deckLimited 3 $ (event "26018" "Defiance" 0 HeroInterrupt [Defense] [Energy] Protection)
    { cdAbilitySubTypes = [Ability.Defense]
    , cdResponseWindow = Just (BoostCardFlipped When (EnemyAttackingCharacter (IdentityCharacters You)))
    }

reboot :: CardDef
reboot = deckLimited 3 $ basicEvent "26024" "Reboot" 1 Action [] [Physical]

joiningForces :: CardDef
joiningForces = deckLimited 3 $ (event "26035" "Joining Forces" 4 HeroAction [] [Energy] Leadership)
    { cdKeywords = fromList [Alliance]
    }

meditation :: CardDef
meditation = deckLimited 3 $ (basicEvent "26036" "Meditation" 0 AlterEgoAction [] [Mental])
    { cdAbilityCost = ExhaustCost
    , cdCriteria = CardPlayAfterExhaustingHasTarget 3
    }


-- Sinister Motives

ghostKick :: CardDef
ghostKick = (identityEvent "27002" "Ghost Kick" 2 HeroResponse [Attack] [Physical])
    { cdDeckLimit = Just 3
    , cdLimit = Just (MaxPerWindow 1)
    , cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    , cdResponseWindow = Just (WindowOneOf [MakesBasicAttack After You, MakesBasicThwart After You, HeroDefended After You AnyEnemy])
    }

parentalGuidance :: CardDef
parentalGuidance = (identityEvent "27003" "Parental Guidance" 0 AlterEgoAction [] [Energy])
    { cdDeckLimit = Just 1
    }

phantomFlip :: CardDef
phantomFlip = (identityEvent "27004" "Phantom Flip" 2 HeroResponse [Thwart] [Energy])
    { cdDeckLimit = Just 3
    , cdLimit = Just (MaxPerWindow 1)
    , cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists SchemeYouCanThwart]
    , cdResponseWindow = Just (WindowOneOf [MakesBasicAttack After You, MakesBasicThwart After You, HeroDefended After You AnyEnemy])
    }

pirouetteAndPunch :: CardDef
pirouetteAndPunch = (identityEvent "27005" "Pirouette and Punch" 2 HeroInterrupt [Skill] [Mental])
    { cdDeckLimit = Just 2
    , cdResponseWindow = Just (EncounterCardRevealedFromDeck When AnyEncounterCard)
    }

webBinding :: CardDef
webBinding = (identityEvent "27006" "Web Binding" 2 HeroInterrupt [Superpower] [Mental])
    { cdDeckLimit = Just 2
    , cdResponseWindow = Just (EnemyActivates When AnyEnemy AnyIdentity)
    , cdKeywords = fromList [Requirement [Mental]]
    }

jumpFlip :: CardDef
jumpFlip = (event "27014" "Jump Flip" 1 HeroInterrupt [Defense] [Physical] Protection)
    { cdDeckLimit = Just 3
    , cdResponseWindow = Just (WouldTakeDamage You FromAnyDamageSource AnyValue)
    , cdAbilitySubTypes = [Ability.Defense]
    }

returnTheFavor :: CardDef
returnTheFavor = (event "27015" "Return the Favor" 0 HeroAction [Attack] [Energy] Protection)
    { cdDeckLimit = Just 3
    , cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttackVillain
    , cdAbilityCost = RevealEncounterTreacheryCost
    }

whatDoesntKillMe :: CardDef
whatDoesntKillMe = (event "27016" "What Doesn't Kill Me" 2 HeroAction [] [Physical] Protection)
    { cdDeckLimit = Just 3
    , cdAbilityCost = HealCost 2
    , cdKeywords = fromList [Requirement [Physical]]
    , cdCriteria = SelfMatches ExhaustedIdentity
    }

acrossTheSpiderVerse :: CardDef
acrossTheSpiderVerse = (basicEvent "27018" "Across the Spider-Verse" 2 HeroAction [] [Wild])
    { cdDeckLimit = Just 1
    , cdAbilityCost = ExhaustCardsWithTraitCost WebWarrior 1 1
    , cdCriteria = ExtendedCardExists (InDiscardOf You (BasicCardMatches (CardWithType AllyType <> CardWithTrait WebWarrior)))
    }

youngLove :: CardDef
youngLove = (basicEvent "27019" "Young Love" 1 AlterEgoAction [] [Wild])
    { cdDeckLimit = Just 1
    , cdTeamUp = Just ("Gwen Stacy", "Miles Morales")
    , cdCriteria = AnyCriteria [CharacterExists (IdentityCharacters (IdentityWithTitle "Gwen Stacy")), AllyExists (AllyNamedAs "Gwen Stacy")] <> AnyCriteria [CharacterExists (IdentityCharacters (IdentityWithTitle "Miles Morales")), AllyExists (AllyNamedAs "Miles Morales")]
    , cdAlternateCardCodes = ["27050"]
    }

arachnobatics :: CardDef
arachnobatics = (identityEvent "27031" "Arachnobatics" 1 HeroAction [Attack] [Mental])
    { cdDeckLimit = Just 2
    , cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

doubleLife :: CardDef
doubleLife = (identityEvent "27032" "Double Life" 1 Action [] [Energy])
    { cdDeckLimit = Just 2
    , cdLimit = Just (MaxPerRound 1)
    }

swingIn :: CardDef
swingIn = (identityEvent "27033" "Swing In" 2 HeroAction [Thwart] [Energy])
    { cdDeckLimit = Just 2
    , cdAbilitySubTypes = [Ability.Thwart]
    , cdCriteria = AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists SchemeYouCanThwart]
    }

webShot :: CardDef
webShot = (identityEvent "27034" "Web-Shot" 2 HeroAction [Attack] [Mental])
    { cdDeckLimit = Just 3
    , cdAbilitySubTypes = [Ability.Attack]
    , cdCriteria = canAttack
    }

homelandIntervention :: CardDef
homelandIntervention = (event "27042" "Homeland Intervention" 0 Action [Shield, Tactic] [Energy] Justice)
    { cdDeckLimit = Just 3
    , cdAbilityCost = ExhaustCardsWithTraitCost Shield 1 3
    , cdCriteria = SchemeExists ThwartableScheme
    }

globalLogistics :: CardDef
globalLogistics = (event "27043" "Global Logistics" 0 Action [Shield, Tactic] [Physical] Justice)
    { cdDeckLimit = Just 3
    , cdAbilityCost = ExhaustCardsWithTraitCost Shield 1 1
    }

-- Nova

forcefieldProjection :: CardDef
forcefieldProjection = (identityEvent "28003" "Forcefield Projection" 1 HeroInterrupt [Superpower] [Physical])
  { cdResponseWindow = Just (CharacterWouldTakeAttackDamage (CharacterControlledBy AnyIdentity))
  , cdDeckLimit = Just 2
  }

lightspeedFlight :: CardDef
lightspeedFlight = (identityEvent "28004" "Lightspeed Flight" 2 HeroAction [Superpower, Thwart] [Mental])
  { cdAbilitySubTypes = [Ability.Thwart]
  , cdCriteria = AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists SchemeYouCanThwart]
  , cdDeckLimit = Just 3
  , cdDoubleWildResources = True
  }

potShot :: CardDef
potShot = (identityEvent "28005" "Pot Shot" 2 HeroAction [Attack, Superpower] [Energy])
  { cdAbilitySubTypes = [Ability.Attack]
  , cdCriteria = canAttack
  , cdDeckLimit = Just 3
  , cdDoubleWildResources = True
  }

unleashNovaForce :: CardDef
unleashNovaForce = (identityEvent "28006" "Unleash Nova Force" 1 HeroAction [Superpower] [Energy])
  { cdDeckLimit = Just 2
  , cdLimit = Just (MaxPerRound 1)
  }

pitchback :: CardDef
pitchback = (event "28012" "Pitchback" 1 HeroResponse [Aerial, Attack] [Physical] Aggression)
  { cdAbilitySubTypes = [Ability.Attack]
  , cdResponseWindow = Just (IdentityAttacked After You AnyEnemy)
  , cdDeckLimit = Just 3
  , cdCriteria = SelfMatches (IdentityWithTrait Aerial) <> canAttack
  }

noQuarter :: CardDef
noQuarter = (event "28013" "No Quarter" 2 HeroAction [Attack] [Mental] Aggression)
  { cdAbilitySubTypes = [Ability.Attack]
  , cdCriteria = canAttack
  , cdDeckLimit = Just 3
  , cdKeywords = fromList [Requirement [Physical]]
  }

oneByOne :: CardDef
oneByOne = (event "28014" "One by One" 1 HeroAction [Attack] [Energy] Aggression)
  { cdAbilitySubTypes = [Ability.Attack]
  , cdCriteria = canAttack
  , cdDeckLimit = Just 3
  }

yawAndRoll :: CardDef
yawAndRoll = (event "28026" "Yaw and Roll" 1 HeroResponse [Aerial, Thwart] [Energy] Justice)
  { cdAbilitySubTypes = [Ability.Thwart]
  , cdResponseWindow = Just (HeroThwarted After You)
  , cdDeckLimit = Just 3
  , cdCriteria = SelfMatches (IdentityWithTrait Aerial) <> AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists SchemeYouCanThwart]
  }


-- Ironheart

flyOver :: CardDef
flyOver = (identityEvent "29005" "Fly Over" 2 HeroAction [Thwart] [Mental])
  { cdDeckLimit = Just 2
  , cdUnique = False
  , cdAbilitySubTypes = [Ability.Thwart]
  , cdCriteria = AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists SchemeYouCanThwart]
  }

photonBeam :: CardDef
photonBeam = (identityEvent "29006" "Photon Beam" 2 HeroAction [Attack] [Energy])
  { cdDeckLimit = Just 3
  , cdUnique = False
  , cdAbilitySubTypes = [Ability.Attack]
  , cdCriteria = canAttack
  }

newAndImproved :: CardDef
newAndImproved = (identityEvent "29007" "New and Improved" 3 HeroAction [Skill] [Mental])
  { cdDeckLimit = Just 2
  , cdUnique = False
  }

sectorScan :: CardDef
sectorScan = (identityEvent "29008" "Sector Scan" 3 HeroAction [] [Energy])
  { cdDeckLimit = Just 1
  , cdUnique = False
  }

goAllOut :: CardDef
goAllOut = (event "29017" "Go All Out" 2 HeroAction [Attack] [Mental] Leadership)
  { cdDeckLimit = Just 3
  , cdUnique = False
  , cdAbilitySubTypes = [Ability.Attack]
  , cdCriteria = canAttack
  , cdAbilityCost = ExhaustCost
  , cdKeywords = fromList [Requirement [Energy]]
  }

pushAhead :: CardDef
pushAhead = (event "29018" "Push Ahead" 3 HeroAction [Thwart] [Energy] Leadership)
  { cdDeckLimit = Just 3
  , cdUnique = False
  , cdAbilitySubTypes = [Ability.Thwart]
  , cdCriteria = AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists SchemeYouCanThwart]
  , cdAbilityCost = ExhaustCost
  , cdKeywords = fromList [Requirement [Mental]]
  }

goForChampions :: CardDef
goForChampions = (basicEvent "29025" "\"Go for Champions!\"" 3 HeroAction [] [Wild])
  { cdDeckLimit = Just 1
  , cdUnique = False
  , cdCriteria = SelfMatches (IdentityWithTrait Champion)
  }


-- Spider-Ham

hamItUp :: CardDef
hamItUp = (identityEvent "30003" "Ham It Up" 0 HeroAction [Cartoon, Thwart] [Mental])
  { cdDeckLimit = Just 2
  , cdUnique = False
  , cdAbilitySubTypes = [Ability.Thwart]
  , cdCriteria = AnyCriteria [SelfMatches ConfusedIdentity, SelfMatches (IdentityWithCounters $ AtLeast $ Static 1) <> SchemeExists SchemeYouCanThwart]
  }

hogwashed :: CardDef
hogwashed = (identityEvent "30004" "Hogwashed" 1 HeroAction [Cartoon] [Energy])
  { cdDeckLimit = Just 1
  , cdUnique = False
  , cdAbilityCost = IdentityCountersCost 1
  , cdCriteria = AnyCriteria [MinionExists AnyMinion, SchemeExists (NotScheme MainScheme <> ThwartableScheme)]
  }

iDontThinkSo :: CardDef
iDontThinkSo = (identityEvent "30005" "\"I Don't Think So!\"" 1 HeroInterrupt [Cartoon] [Energy])
  { cdDeckLimit = Just 1
  , cdUnique = False
  , cdAbilityCost = IdentityCountersCost 1
  , cdCriteria = CanCancelRevealedEncounterCard
  , cdResponseWindow = Just (EncounterCardRevealedFromDeckBy When You AnyEncounterCard)
  }

petulantPig :: CardDef
petulantPig = (identityEvent "30006" "Petulant Pig" 0 HeroAction [Cartoon] [Mental])
  { cdDeckLimit = Just 2
  , cdUnique = False
  }

swingingWebPig :: CardDef
swingingWebPig = (identityEvent "30007" "Swinging Web Pig" 3 HeroAction [Cartoon, Attack] [Energy])
  { cdDeckLimit = Just 3
  , cdUnique = False
  , cdAbilitySubTypes = [Ability.Attack]
  , cdCriteria = canAttack
  }

evenTheOdds :: CardDef
evenTheOdds = reprintedAs ["40052"] $ (event "30014" "Even the Odds" 2 HeroAction [Thwart] [Energy] Justice)
  { cdDeckLimit = Just 3
  , cdUnique = False
  , cdAbilitySubTypes = [Ability.Thwart]
  , cdCriteria = AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists (NotScheme MainScheme <> SchemeYouCanThwart)]
  , cdKeywords = fromList [Requirement [Energy]]
  }


-- SP//dr

allSystemsGo :: CardDef
allSystemsGo = (identityEvent "31004" "All Systems Go!" 1 HeroAction [] [Energy])
  { cdUnique = False
  , cdDeckLimit = Just 3
  }

rapidDeployment :: CardDef
rapidDeployment = (identityEvent "31005" "Rapid Deployment" 2 HeroAction [Thwart] [Mental])
  { cdUnique = False
  , cdDeckLimit = Just 2
  , cdAbilitySubTypes = [Ability.Thwart]
  , cdCriteria = AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists SchemeYouCanThwart]
  }

webTrap :: CardDef
webTrap = (identityEvent "31006" "Web-Trap" 2 HeroAction [Attack] [Energy])
  { cdUnique = False
  , cdDeckLimit = Just 2
  , cdAbilitySubTypes = [Ability.Attack]
  , cdCriteria = canAttack
  }

repurpose :: CardDef
repurpose = (event "31016" "Repurpose" 0 HeroAction [Skill] [Energy] Protection)
  { cdUnique = False
  , cdAbilityCost = DiscardUpgradeCost (UpgradeWithTrait Tech)
  , cdDeckLimit = Just 3
  }

thwipThwip :: CardDef
thwipThwip = (event "31017" "Thwip Thwip!" 2 HeroAction [Superpower] [Mental] Protection)
  { cdUnique = False
  , cdAbilityCost = DamageCharacterCost (CharacterWithTrait WebWarrior <> CharacterControlledBy You) 1
  , cdCriteria = EnemyExists AnyEnemy
  , cdDeckLimit = Just 3
  }

limitlessStamina :: CardDef
limitlessStamina = (basicEvent "31023" "Limitless Stamina" 1 HeroAction [Superpower] [Physical])
  { cdUnique = False
  , cdCriteria = SelfMatches (IdentityWithPrintedHitPoints $ AtLeast $ Static 14)
  , cdDeckLimit = Just 3
  }

-- Mutant Genesis

madeOfRage :: CardDef
madeOfRage = (identityEvent "32007" "Made of Rage" 0 HeroInterrupt [Superpower] [Energy])
  { cdDeckLimit = Just 2
  , cdAbilityCost = DiscardToughCost 1
  , cdResponseWindow = Just (MakesBasicAttack When You)
  }

-- Mutant Genesis

steelFist :: CardDef
steelFist = (identityEvent "32008" "Steel Fist" 2 HeroAction [Attack, Superpower] [Physical])
  { cdDeckLimit = Just 3
  , cdAbilitySubTypes = [Ability.Attack]
  }

-- Mutant Genesis

bulletproofProtector :: CardDef
bulletproofProtector = (identityEvent "32009" "Bulletproof Protector" 0 HeroAction [Superpower] [Physical])
  { cdDeckLimit = Just 2
  , cdAbilityCost = DiscardToughCost 1
  }

-- Mutant Genesis

armorUp :: CardDef
armorUp = (identityEvent "32010" "Armor Up" 0 AlterEgoInterrupt [Superpower] [Physical])
  { cdDeckLimit = Just 2
  , cdResponseWindow = Just (VillainWouldActivate AnyIdentity)
  }

-- Mutant Genesis

powerfulPunch :: CardDef
powerfulPunch = reprintedAs ["48017"] $ (event "32014" "Powerful Punch" 2 HeroInterrupt [Attack, Defense] [Physical] Protection)
  { cdDeckLimit = Just 3
  , cdAbilitySubTypes = [Ability.Attack, Ability.Defense]
  , cdResponseWindow = Just (EnemyWouldAttack AnyEnemy AnyIdentity)
  }

-- Mutant Genesis

mutantProtectors :: CardDef
mutantProtectors = (event "32017" "Mutant Protectors" 1 HeroInterrupt [Defense] [Energy] Protection)
  { cdDeckLimit = Just 3
  , cdAbilitySubTypes = [Ability.Defense]
  , cdCriteria = SelfMatches (IdentityWithTrait XMen)
      <> ExtendedCardExists (InHandOf You $ BasicCardMatches $ CardWithType AllyType <> CardWithTrait XMen)
  , cdResponseWindow = Just (EnemyWouldAttack AnyEnemy AnyIdentity)
  }

-- Mutant Genesis

shadowAndSteel :: CardDef
shadowAndSteel = reprintedAs ["32050"] $ (basicEvent "32021" "Shadow and Steel" 2 HeroInterrupt [Attack, Defense] [Physical])
  { cdDeckLimit = Just 1
  , cdAbilitySubTypes = [Ability.Attack, Ability.Defense]
  , cdTeamUp = Just ("Colossus", "Shadowcat")
  , cdResponseWindow = Just (EnemyWouldAttack AnyEnemy AnyIdentity)
  }

-- Mutant Genesis

shadowcatSurprise :: CardDef
shadowcatSurprise = (identityEvent "32037" "Shadowcat Surprise" 2 HeroAction [Attack, Superpower] [Physical])
  { cdDeckLimit = Just 3
  , cdAbilitySubTypes = [Ability.Attack]
  }

-- Mutant Genesis

phasedStrike :: CardDef
phasedStrike = (identityEvent "32038" "Phased Strike" 3 HeroAction [Attack, Superpower] [Energy])
  { cdDeckLimit = Just 2
  , cdAbilitySubTypes = [Ability.Attack]
  }

-- Mutant Genesis

airwalk :: CardDef
airwalk = (identityEvent "32039" "Airwalk" 1 HeroAction [Superpower, Thwart] [Mental])
  { cdDeckLimit = Just 2
  , cdAbilitySubTypes = [Ability.Thwart]
  }

-- Mutant Genesis

quickShift :: CardDef
quickShift = (identityEvent "32040" "Quick Shift" 0 HeroInterrupt [Defense, Superpower] [Energy])
  { cdDeckLimit = Just 2
  , cdAbilitySubTypes = [Ability.Defense]
  , cdResponseWindow = Just (EnemyWouldAttack AnyEnemy AnyIdentity)
  }

-- Mutant Genesis

teamStrike :: CardDef
teamStrike = (event "32045" "Team Strike" 1 HeroAction [Attack] [Energy] Aggression)
  { cdDeckLimit = Just 3
  , cdAbilitySubTypes = [Ability.Attack]
  , cdCriteria = SelfMatches (IdentityWithTrait XMen)
  , cdAbilityCost = ExhaustCost <> ExhaustAlliesWithTraitCost XMen 0 Nothing
  }

-- Wolverine

berserkerBarrage :: CardDef
berserkerBarrage = (identityEvent "35008" "Berserker Barrage" 2 HeroAction [Attack] [Physical]) {cdDeckLimit = Just 2, cdAbilitySubTypes = [Ability.Attack]}

sliceAndDice :: CardDef
sliceAndDice = (identityEvent "35009" "Slice and Dice" 3 HeroAction [Attack] [Energy]) {cdDeckLimit = Just 2, cdCriteria = canAttack}

lungingStrike :: CardDef
lungingStrike = (identityEvent "35010" "Lunging Strike" 3 HeroAction [Attack] [Physical]) {cdDeckLimit = Just 2, cdAbilitySubTypes = [Ability.Attack]}

trackByScent :: CardDef
trackByScent = (identityEvent "35011" "Track by Scent" 2 HeroAction [Superpower, Thwart] [Mental]) {cdDeckLimit = Just 2, cdAbilitySubTypes = [Ability.Thwart]}

regenerativeHealing :: CardDef
regenerativeHealing = (identityEvent "35012" "Regenerative Healing" 1 Action [Superpower] [Energy]) {cdDeckLimit = Just 2, cdCriteria = AnyCriteria [SelfMatches (IdentityWithAnyDamage <> IdentityCanBeHealed), CharacterExists (IdentityCharacters You <> CharacterWithStatus StunnedStatus), CharacterExists (IdentityCharacters You <> CharacterWithStatus ConfusedStatus)]}

outtaMyWay :: CardDef
outtaMyWay = (event "35017" "Outta My Way!" 2 HeroAction [Attack] [Physical] Aggression) {cdDeckLimit = Just 3, cdAbilitySubTypes = [Ability.Attack]}

precisionStrike :: CardDef
precisionStrike = (event "35018" "Precision Strike" 1 HeroAction [Attack] [Mental] Aggression) {cdDeckLimit = Just 3, cdAbilitySubTypes = [Ability.Attack]}

fastballSpecial :: CardDef
fastballSpecial = (basicEvent "35023" "Fastball Special" 1 HeroAction [Aerial, Attack] [Energy]) {cdDeckLimit = Just 1, cdTeamUp = Just ("Colossus", "Wolverine"), cdCriteria = teamUp "Colossus" "Wolverine", cdAbilitySubTypes = [Ability.Attack]}


-- Phoenix

telekineticAttack :: CardDef
telekineticAttack = (identityEvent "34010" "Telekinetic Attack" 3 HeroAction [Attack, Psionic] [Physical]) {cdDeckLimit = Just 2, cdAbilitySubTypes = [Ability.Attack], cdCriteria = AnyCriteria [SelfMatches StunnedIdentity, EnemyExists AttackableEnemy]}

psychicBlast :: CardDef
psychicBlast = (identityEvent "34011" "Psychic Blast" 2 HeroAction [Psionic] [Energy]) {cdDeckLimit = Just 2}

telepathicTrickery :: CardDef
telepathicTrickery = (identityEvent "34012" "Telepathic Trickery" 2 HeroAction [Psionic, Thwart] [Mental]) {cdDeckLimit = Just 2, cdCriteria = AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists (ThwartableScheme <> SchemeWithoutThwartPrevention), SelfMatches (IdentityWithTrait Unleashed) <> AnyCriteria [EnemyExists EnemyCanBeStunned, EnemyExists EnemyCanBeConfused]], cdAbilitySubTypes = [Ability.Thwart]}

phoenixFirebird :: CardDef
phoenixFirebird = (identityEvent "34013" "Phoenix Firebird" 1 HeroAction [Phoenix] [Energy]) {cdDeckLimit = Just 2}

psychicManipulation :: CardDef
psychicManipulation = (event "34017" "Psychic Manipulation" 3 Interrupt [Psionic, Thwart] [Energy] Justice) {cdDeckLimit = Just 3, cdAbilitySubTypes = [Ability.Thwart], cdResponseWindow = Just (VillainSchemed When AnyVillain), cdCriteria = SelfMatches (IdentityWithTrait Psionic) <> AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists (MainScheme <> SchemeYouCanThwart)]}

mutantPeacekeepers :: CardDef
mutantPeacekeepers = (event "34018" "Mutant Peacekeepers" 1 HeroAction [Thwart] [Physical] Justice) {cdDeckLimit = Just 3, cdAbilitySubTypes = [Ability.Thwart], cdAbilityCost = ExhaustCost <> ExhaustAlliesWithTraitCost XMen 0 Nothing, cdCriteria = SelfMatches (IdentityWithTrait XMen) <> AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists SchemeYouCanThwart]}

psychicRapport :: CardDef
psychicRapport = reprintedAs ["34023"] $ (basicEvent "33023" "Psychic Rapport" 2 HeroAction [] [Energy]) {cdDeckLimit = Just 1, cdTeamUp = Just ("Cyclops", "Phoenix"), cdCriteria = teamUp "Cyclops" "Phoenix"}

psychicAssault :: CardDef
psychicAssault = (event "34032" "Psychic Assault" 2 HeroAction [Attack, Psionic] [Energy] Aggression) {cdDeckLimit = Just 3, cdAbilitySubTypes = [Ability.Attack], cdCriteria = SelfMatches (IdentityWithTrait Psionic) <> AnyCriteria [SelfMatches StunnedIdentity, EnemyExists AttackableEnemy]}

psychicMisdirection :: CardDef
psychicMisdirection = (event "34033" "Psychic Misdirection" 2 HeroInterrupt [Defense, Psionic] [Energy] Protection) {cdDeckLimit = Just 3, cdAbilitySubTypes = [Ability.Defense], cdResponseWindow = Just (EnemyAttacked When AnyEnemy You), cdCriteria = SelfMatches (IdentityWithTrait Psionic)}

psychicKicker :: CardDef
psychicKicker = (event "34034" "Psychic Kicker" 0 HeroAction [Psionic] [Energy] Leadership) {cdDeckLimit = Just 3, cdCriteria = SelfMatches (IdentityWithTrait Psionic)}

soulSisters :: CardDef
soulSisters = (basicEvent "34035" "Soul Sisters" 1 HeroAction [] [Energy]) {cdDeckLimit = Just 1, cdTeamUp = Just ("Phoenix", "Storm"), cdCriteria = teamUp "Phoenix" "Storm"}


-- Cyclops

fullBlast :: CardDef
fullBlast = (identityEvent "33008" "Full Blast" 1 HeroInterrupt [Superpower] [Physical]) { cdDeckLimit = Just 1, cdResponseWindow = Just (UsesAbility When You 1), cdAbilityCost = ExhaustCost, cdCriteria = SelfMatches (IdentityWithTitle "Cyclops") }

ricochetBeam :: CardDef
ricochetBeam = (identityEvent "33009" "Ricochet Beam" 2 HeroAction [Attack, Superpower] [Energy]) { cdDeckLimit = Just 2, cdAbilitySubTypes = [Ability.Attack], cdCriteria = EnemyExists AttackableEnemy }

tacticalBrilliance :: CardDef
tacticalBrilliance = (identityEvent "33010" "Tactical Brilliance" 2 HeroAction [Thwart] [Mental]) { cdDeckLimit = Just 3, cdAbilitySubTypes = [Ability.Thwart], cdCriteria = SchemeExists ThwartableScheme }

gameTime :: CardDef
gameTime = (basicEvent "33022" "Game Time" 0 HeroAction [Tactic] [Energy]) { cdDeckLimit = Just 3, cdCriteria = AllyExists (AllyWithUpgrade (UpgradeWithTrait Training)) }


-- Storm

weatherGoddess :: CardDef
weatherGoddess = (identityEvent "36009" "Weather Goddess" 0 HeroAction [Superpower] [Energy]) { cdDeckLimit = Just 3 }

torrentialRain :: CardDef
torrentialRain = (identityEvent "36010" "Torrential Rain" 2 HeroAction [Superpower, Thwart] [Mental]) { cdDeckLimit = Just 3, cdAbilitySubTypes = [Ability.Thwart], cdCriteria = SchemeExists ThwartableScheme }

lightningBolt :: CardDef
lightningBolt = (identityEvent "36011" "Lightning Bolt" 3 HeroAction [Attack, Superpower] [Energy]) { cdDeckLimit = Just 2, cdAbilitySubTypes = [Ability.Attack], cdCriteria = EnemyExists AttackableEnemy }

flashFreeze :: CardDef
flashFreeze = (identityEvent "36012" "Flash Freeze" 1 HeroInterrupt [Defense, Superpower] [Physical]) { cdDeckLimit = Just 2, cdAbilitySubTypes = [Ability.Defense], cdResponseWindow = Just (EnemyAttacked When VillainEnemy You) }

blastOfWind :: CardDef
blastOfWind = (identityEvent "36013" "Blast of Wind" 3 HeroAction [Superpower] [Physical]) { cdDeckLimit = Just 2 }

toMeMyXMen :: CardDef
toMeMyXMen = (event "36020" "\"To Me, My X-Men!\"" 1 HeroAction [Tactic] [Energy] Leadership) { cdDeckLimit = Just 3, cdCriteria = SelfMatches (IdentityWithTrait XMen) }

-- Gambit

chargedCard :: CardDef
chargedCard = (identityEvent "37006" "Charged Card" 2 HeroAction [Attack, Superpower] [Physical]) { cdDeckLimit = Just 3, cdAbilitySubTypes = [Ability.Attack], cdCriteria = AnyCriteria [SelfMatches StunnedIdentity, EnemyExists AttackableEnemy] }

royalFlush :: CardDef
royalFlush = (identityEvent "37007" "Royal Flush" 3 HeroAction [Attack, Superpower] [Energy]) { cdDeckLimit = Just 2, cdAbilitySubTypes = [Ability.Attack], cdCriteria = AnyCriteria [SelfMatches StunnedIdentity, EnemyExists AttackableEnemy] }

naturalAgility :: CardDef
naturalAgility = (identityEvent "37008" "Natural Agility" 0 HeroInterrupt [Defense, Superpower] [Energy]) { cdDeckLimit = Just 2, cdAbilitySubTypes = [Ability.Defense], cdAbilityCost = PlaceIdentityCountersCost 1, cdResponseWindow = Just (HeroDefended When You AnyEnemy) }

creoleCharmer :: CardDef
creoleCharmer = (identityEvent "37009" "Creole Charmer" 2 AlterEgoAction [Thwart] [Energy]) { cdDeckLimit = Just 2, cdAbilitySubTypes = [Ability.Thwart], cdCriteria = AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists SchemeYouCanThwart] }

breakingAndEntering :: CardDef
breakingAndEntering = (event "37015" "Breaking and Entering" 2 Action [Thwart] [Energy] Justice) { cdDeckLimit = Just 3, cdAbilitySubTypes = [Ability.Thwart], cdCriteria = AnyCriteria [SelfMatches (IdentityWithTrait Spy), SelfMatches (IdentityWithTrait Thief)] <> AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists SchemeYouCanThwart] }

beautyAndTheThief :: CardDef
beautyAndTheThief = reprintedAs ["38020"] $ (basicEvent "37019" "Beauty and the Thief" 2 HeroAction [Attack, Thwart] [Energy]) { cdDeckLimit = Just 1, cdAbilitySubTypes = [Ability.Attack, Ability.Thwart], cdCriteria = teamUp "Gambit" "Rogue", cdTeamUp = Just ("Gambit", "Rogue") }

mutantEducation :: CardDef
mutantEducation = (basicEvent "37021" "Mutant Education" 0 AlterEgoAction [] [Energy]) { cdDeckLimit = Just 3, cdCriteria = SelfMatches (IdentityWithTrait Mutant) }

xMenInstruction :: CardDef
xMenInstruction = (basicEvent "37031" "X-Men Instruction" 0 AlterEgoAction [] [Energy]) { cdDeckLimit = Just 3, cdCriteria = SelfMatches (IdentityWithTrait Mutant) }


-- Rogue

goinRogue :: CardDef
goinRogue = (identityEvent "38005" "Goin' Rogue" 2 HeroAction [Superpower, Thwart] [Mental]) { cdDeckLimit = Just 3, cdAbilitySubTypes = [Ability.Thwart], cdCriteria = AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists SchemeYouCanThwart] }

southernCross :: CardDef
southernCross = (identityEvent "38006" "Southern Cross" 3 HeroAction [Attack, Superpower] [Physical]) { cdDeckLimit = Just 3, cdAbilitySubTypes = [Ability.Attack], cdCriteria = AnyCriteria [SelfMatches StunnedIdentity, EnemyExists AttackableEnemy] }

energyTransfer :: CardDef
energyTransfer = (identityEvent "38007" "Energy Transfer" 2 HeroAction [Superpower] [Energy]) { cdAbilityCost = TouchedDamageCost 2, cdDeckLimit = Just 2 }

bulletproofBelle :: CardDef
bulletproofBelle = (identityEvent "38008" "Bulletproof Belle" 1 HeroInterrupt [Defense, Superpower] [Physical]) { cdDeckLimit = Just 2, cdAbilitySubTypes = [Ability.Defense], cdResponseWindow = Just (EnemyAttacked When (EnemyWithUpgrade $ UpgradeIs Upgrades.touched <> UpgradeControlledBy You) AnyIdentity) }

superpowerAdaptation :: CardDef
superpowerAdaptation = (identityEvent "38009" "Superpower Adaptation" 0 HeroAction [Superpower] [Mental]) { cdDeckLimit = Just 3 }

notToday :: CardDef
notToday = (event "38016" "Not Today!" 1 HeroInterrupt [Defense] [Mental] Protection) { cdDeckLimit = Just 3, cdAbilitySubTypes = [Ability.Defense], cdResponseWindow = Just (HeroDefended When You AnyEnemy) }

-- NeXt Evolution: Cable

cable40002 :: CardDef
cable40002 = (identityEvent "40002" "Bodyslide" 0 NEAbility.Action [] [NE.Energy]) { cdUnique = False, cdDeckLimit = Just 1 }

cable40003 :: CardDef
cable40003 = (identityEvent "40003" "Mind Scan" 2 NEAbility.HeroAction [NE.Psionic, NE.Superpower, NE.Thwart] [NE.Mental]) { cdUnique = False, cdDeckLimit = Just 3 , cdAbilitySubTypes = [NEAbility.Thwart] }

cable40004 :: CardDef
cable40004 = (identityEvent "40004" "Precognition" 0 NEAbility.HeroAction [NE.Psionic, NE.Superpower] [NE.Mental]) { cdUnique = False, cdDeckLimit = Just 1 }

cable40005 :: CardDef
cable40005 = (identityEvent "40005" "Telekinetic Blast" 3 NEAbility.HeroAction [NE.Attack, NE.Psionic, NE.Superpower] [NE.Energy]) { cdUnique = False, cdDeckLimit = Just 2 , cdAbilitySubTypes = [NEAbility.Attack] }

cable40017 :: CardDef
cable40017 = (event "40017" "Mission Planning" 2 NEAbility.HeroAction [] [NE.Mental] NE.Leadership) { cdUnique = False, cdDeckLimit = Just 3 , cdCriteria = VictoryDisplayHasSideScheme }

cable40026 :: CardDef
cable40026 = reprintedAs ["44031"] $ (basicEvent "40026" "Frenemies" 1 NEAbility.HeroAction [NE.Thwart] [NE.Wild]) { cdUnique = False, cdDeckLimit = Just 1 , cdAbilitySubTypes = [NEAbility.Thwart] , cdTeamUp = Just ("Cable", "Deadpool") , cdCriteria = teamUp "Cable" "Deadpool" <> SchemeExists (SchemeWithThreat (GreaterThan (Static 0))) }

-- NeXt Evolution: Domino
aGoodWorkout :: CardDef
aGoodWorkout = CardDef
  { cdCardCode = "40040", cdName = "A Good Workout", cdCost = Just 2
  , cdTraits = fromList [NET.Attack, NET.Superpower], cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just NEAbility.HeroAction, cdAbilitySubTypes = [NEAbility.Attack]
  , cdUnique = False, cdAspect = Nothing, cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NER.Wild)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 2, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution: Domino
luckBeaLady :: CardDef
luckBeaLady = CardDef
  { cdCardCode = "40041", cdName = "Luck Be a Lady", cdCost = Just 1
  , cdTraits = fromList [NET.Superpower], cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just NEAbility.HeroAction, cdAbilitySubTypes = []
  , cdUnique = False, cdAspect = Nothing, cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NER.Wild)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution: Domino
rightPlaceRightTime :: CardDef
rightPlaceRightTime = CardDef
  { cdCardCode = "40042", cdName = "Right Place, Right Time", cdCost = Just 2
  , cdTraits = fromList [NET.Superpower, NET.Thwart], cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just NEAbility.HeroAction, cdAbilitySubTypes = [NEAbility.Thwart]
  , cdUnique = False, cdAspect = Nothing, cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NER.Wild)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 2, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution: Domino
teamInvestigation :: CardDef
teamInvestigation = CardDef
  { cdCardCode = "40053", cdName = "Team Investigation", cdCost = Just 2
  , cdTraits = fromList [], cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just NEAbility.HeroAction, cdAbilitySubTypes = []
  , cdUnique = False, cdAspect = Just NEA.Justice, cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NER.Mental)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution: Domino
thePosse :: CardDef
thePosse = CardDef
  { cdCardCode = "40058", cdName = "The Posse", cdCost = Just 2
  , cdTraits = fromList [NET.Posse], cdKeywords = fromList []
  , cdCardType = EventType, cdAbilityType = Just NEAbility.HeroAction, cdAbilitySubTypes = []
  , cdUnique = False, cdAspect = Just NEA.Neutral, cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NER.Wild)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- Psylocke

flurryOfBlades :: CardDef
flurryOfBlades = CardDef
  { cdCardCode = "41004", cdName = "Flurry of Blades", cdCost = Just 3
  , cdTraits = fromList [PsyTrait.Attack, PsyTrait.Psionic], cdKeywords = fromList []
  , cdCardType = PsyCard.EventType, cdAbilityType = Just PsyAbility.HeroAction, cdAbilitySubTypes = [PsyAbility.Attack]
  , cdUnique = False, cdAspect = Nothing, cdCriteria = PsyCriteria.NoCriteria
  , cdResources = [(PsyCard.PrintedResource, PsyResource.Energy)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

mentalDetection :: CardDef
mentalDetection = CardDef
  { cdCardCode = "41005", cdName = "Mental Detection", cdCost = Just 2
  , cdTraits = fromList [PsyTrait.Psionic, PsyTrait.Thwart], cdKeywords = fromList []
  , cdCardType = PsyCard.EventType, cdAbilityType = Just PsyAbility.HeroAction, cdAbilitySubTypes = [PsyAbility.Thwart]
  , cdUnique = False, cdAspect = Nothing, cdCriteria = PsyCriteria.NoCriteria
  , cdResources = [(PsyCard.PrintedResource, PsyResource.Mental)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

psionicRedirect :: CardDef
psionicRedirect = CardDef
  { cdCardCode = "41006", cdName = "Psionic Redirect", cdCost = Just 2
  , cdTraits = fromList [PsyTrait.Defense, PsyTrait.Psionic], cdKeywords = fromList []
  , cdCardType = PsyCard.EventType, cdAbilityType = Just PsyAbility.HeroInterrupt, cdAbilitySubTypes = [PsyAbility.Defense]
  , cdUnique = False, cdAspect = Nothing, cdCriteria = PsyCriteria.NoCriteria
  , cdResources = [(PsyCard.PrintedResource, PsyResource.Mental)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (PsyWindow.IdentityWouldTakeAttackDamageFrom PsyMatch.AnyEnemy PsyMatch.You), cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 2, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

telepathicSuggestion :: CardDef
telepathicSuggestion = CardDef
  { cdCardCode = "41007", cdName = "Telepathic Suggestion", cdCost = Just 2
  , cdTraits = fromList [PsyTrait.Psionic], cdKeywords = fromList []
  , cdCardType = PsyCard.EventType, cdAbilityType = Just PsyAbility.HeroInterrupt, cdAbilitySubTypes = []
  , cdUnique = False, cdAspect = Nothing, cdCriteria = PsyCriteria.NoCriteria
  , cdResources = [(PsyCard.PrintedResource, PsyResource.Energy)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (PsyWindow.EncounterCardRevealedFromDeckBy PsyWindow.When PsyMatch.You PsyMatch.AnyEncounterCard), cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 2, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

upsideTheHead :: CardDef
upsideTheHead = CardDef
  { cdCardCode = "41015", cdName = "Upside the Head", cdCost = Just 1
  , cdTraits = fromList [PsyTrait.Skill], cdKeywords = fromList []
  , cdCardType = PsyCard.EventType, cdAbilityType = Just PsyAbility.HeroResponse, cdAbilitySubTypes = []
  , cdUnique = False, cdAspect = Just PsyAspect.Justice, cdCriteria = PsyCriteria.NoCriteria
  , cdResources = [(PsyCard.PrintedResource, PsyResource.Mental)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (PsyWindow.BasicAttackDamagedEnemy PsyMatch.You), cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

directedForce :: CardDef
directedForce = CardDef
  { cdCardCode = "41019", cdName = "Directed Force", cdCost = Just 0
  , cdTraits = fromList [PsyTrait.Skill], cdKeywords = fromList []
  , cdCardType = PsyCard.EventType, cdAbilityType = Just PsyAbility.HeroInterrupt, cdAbilitySubTypes = []
  , cdUnique = False, cdAspect = Just PsyAspect.Neutral, cdCriteria = PsyCriteria.NoCriteria
  , cdResources = [(PsyCard.PrintedResource, PsyResource.Physical)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (PsyWindow.MakesKeywordAttack PsyWindow.When PsyMatch.You [PsyKeyword.Overkill, PsyKeyword.Piercing, PsyKeyword.Ranged]), cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Just (PsyCard.MaxPerWindow 1), cdDeckLimit = Just 3, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

soaringHearts :: CardDef
soaringHearts = reprintedAs ["42021"] $ CardDef
  { cdCardCode = "41020", cdName = "Soaring Hearts", cdCost = Just 2
  , cdTraits = fromList [PsyTrait.Aerial, PsyTrait.Psionic], cdKeywords = fromList []
  , cdCardType = PsyCard.EventType, cdAbilityType = Just PsyAbility.HeroAction, cdAbilitySubTypes = []
  , cdUnique = False, cdAspect = Just PsyAspect.Neutral, cdCriteria = teamUp "Angel" "Psylocke"
  , cdResources = [(PsyCard.PrintedResource, PsyResource.Wild)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Just ("Angel", "Psylocke")
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

psiBowAttack :: CardDef
psiBowAttack = CardDef
  { cdCardCode = "41030", cdName = "Psi-Bow Attack", cdCost = Just 2
  , cdTraits = fromList [PsyTrait.Attack, PsyTrait.Psionic], cdKeywords = fromList []
  , cdCardType = PsyCard.EventType, cdAbilityType = Just PsyAbility.HeroAction, cdAbilitySubTypes = [PsyAbility.Attack]
  , cdUnique = False, cdAspect = Just PsyAspect.Aggression, cdCriteria = PsyCriteria.SelfMatches (PsyMatch.HeroIdentity <> PsyMatch.IdentityWithTrait PsyTrait.Psionic)
  , cdResources = [(PsyCard.PrintedResource, PsyResource.Mental)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

psiFlailStrike :: CardDef
psiFlailStrike = CardDef
  { cdCardCode = "41032", cdName = "Psi-Flail Strike", cdCost = Just 2
  , cdTraits = fromList [PsyTrait.Attack, PsyTrait.Psionic], cdKeywords = fromList []
  , cdCardType = PsyCard.EventType, cdAbilityType = Just PsyAbility.HeroResponse, cdAbilitySubTypes = [PsyAbility.Attack]
  , cdUnique = False, cdAspect = Just PsyAspect.Protection, cdCriteria = PsyCriteria.SelfMatches (PsyMatch.HeroIdentity <> PsyMatch.IdentityWithTrait PsyTrait.Psionic)
  , cdResources = [(PsyCard.PrintedResource, PsyResource.Energy)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (PsyWindow.HeroDefended PsyWindow.After PsyMatch.You PsyMatch.AnyEnemy), cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

-- Angel

adaptivePlumage :: CardDef
adaptivePlumage = CardDef
  { cdCardCode = "42003"
  , cdName = "Adaptive Plumage"
  , cdCost = Just 3
  , cdTraits = fromList [AnTrait.Aerial, AnTrait.Attack, AnTrait.Thwart]
  , cdKeywords = fromList []
  , cdCardType = AnCard.EventType
  , cdAbilityType = Just AnAbility.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.AnyCriteria [AnCriteria.SelfMatches (AnMatch.IdentityWithTitle "Angel") <> AnCriteria.AnyCriteria [AnCriteria.SchemeExists AnMatch.SchemeYouCanThwart, AnCriteria.EnemyExists AnMatch.AnyEnemy], AnCriteria.SelfMatches (AnMatch.IdentityWithTitle "Archangel") <> AnCriteria.canAttack]
  , cdResources = [(AnCard.PrintedResource, AnResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

aerialAgility :: CardDef
aerialAgility = CardDef
  { cdCardCode = "42004"
  , cdName = "Aerial Agility"
  , cdCost = Just 1
  , cdTraits = fromList [AnTrait.Aerial, AnTrait.Defense]
  , cdKeywords = fromList []
  , cdCardType = AnCard.EventType
  , cdAbilityType = Just AnAbility.HeroInterrupt
  , cdAbilitySubTypes = [AnAbility.Defense]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = [(AnCard.PrintedResource, AnResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (AnWindow.EnemyAttacksPlayerOrAlly AnWindow.When AnMatch.AnyEnemy AnMatch.AnyIdentity)
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

metamorphosis :: CardDef
metamorphosis = CardDef
  { cdCardCode = "42005"
  , cdName = "Metamorphosis"
  , cdCost = Just 2
  , cdTraits = fromList [AnTrait.Aerial]
  , cdKeywords = fromList []
  , cdCardType = AnCard.EventType
  , cdAbilityType = Just AnAbility.Action
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.SelfMatches AnMatch.IdentityCanChangeForm
  , cdResources = [(AnCard.PrintedResource, AnResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

naturalFlight :: CardDef
naturalFlight = CardDef
  { cdCardCode = "42006"
  , cdName = "Natural Flight"
  , cdCost = Just 2
  , cdTraits = fromList [AnTrait.Aerial, AnTrait.Thwart]
  , cdKeywords = fromList []
  , cdCardType = AnCard.EventType
  , cdAbilityType = Just AnAbility.HeroAction
  , cdAbilitySubTypes = [AnAbility.Thwart]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.AnyCriteria [AnCriteria.SelfMatches (AnMatch.IdentityWithTitle "Angel") <> AnCriteria.SchemeExists (AnMatch.ThwartableSchemeIgnoringCrisis <> AnMatch.SchemeWithoutThwartPrevention), AnCriteria.NotCriteria (AnCriteria.SelfMatches (AnMatch.IdentityWithTitle "Angel")) <> AnCriteria.SchemeExists AnMatch.SchemeYouCanThwart]
  , cdResources = [(AnCard.PrintedResource, AnResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

razorDive :: CardDef
razorDive = CardDef
  { cdCardCode = "42007"
  , cdName = "Razor Dive"
  , cdCost = Just 3
  , cdTraits = fromList [AnTrait.Aerial, AnTrait.Attack]
  , cdKeywords = fromList []
  , cdCardType = AnCard.EventType
  , cdAbilityType = Just AnAbility.HeroAction
  , cdAbilitySubTypes = [AnAbility.Attack]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = canAttack
  , cdResources = [(AnCard.PrintedResource, AnResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

aerialIntervention :: CardDef
aerialIntervention = CardDef
  { cdCardCode = "42014"
  , cdName = "Aerial Intervention"
  , cdCost = Just 0
  , cdTraits = fromList [AnTrait.Aerial]
  , cdKeywords = fromList []
  , cdCardType = AnCard.EventType
  , cdAbilityType = Just AnAbility.Interrupt
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just AnAspect.Protection
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = [(AnCard.PrintedResource, AnResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (AnWindow.CharacterWouldTakeAttackDamage AnMatch.AnyCharacter)
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.ExhaustCharacterWithTraitCost AnTrait.Aerial
  }

taunt :: CardDef
taunt = CardDef
  { cdCardCode = "42016"
  , cdName = "Taunt"
  , cdCost = Just 1
  , cdTraits = fromList [AnTrait.Tactic]
  , cdKeywords = fromList []
  , cdCardType = AnCard.EventType
  , cdAbilityType = Just AnAbility.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just AnAspect.Protection
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = [(AnCard.PrintedResource, AnResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

bombsAway :: CardDef
bombsAway = CardDef
  { cdCardCode = "42029"
  , cdName = "Bombs Away"
  , cdCost = Just 2
  , cdTraits = fromList [AnTrait.Aerial, AnTrait.Tactic]
  , cdKeywords = fromList []
  , cdCardType = AnCard.EventType
  , cdAbilityType = Just AnAbility.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just AnAspect.Aggression
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = [(AnCard.PrintedResource, AnResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.ExhaustCharacterWithTraitCost AnTrait.Aerial
  }

flyingFormation :: CardDef
flyingFormation = CardDef
  { cdCardCode = "42031"
  , cdName = "Flying Formation"
  , cdCost = Just 4
  , cdTraits = fromList [AnTrait.Aerial, AnTrait.Tactic]
  , cdKeywords = fromList [AnKeyword.Alliance]
  , cdCardType = AnCard.EventType
  , cdAbilityType = Just AnAbility.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just AnAspect.Leadership
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = [(AnCard.PrintedResource, AnResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

-- X-23

animalInstinct :: CardDef
animalInstinct = CardDef
  { cdCardCode = "43004"
  , cdName = "Animal Instinct"
  , cdCost = Just 0
  , cdTraits = fromList [AnTrait.Skill]
  , cdKeywords = fromList []
  , cdCardType = AnCard.EventType
  , cdAbilityType = Just AnAbility.HeroInterrupt
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = [(AnCard.PrintedResource, AnResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (AnWindow.MakesBasicThwart AnWindow.When (AnMatch.IdentityWithTitle "X-23"))
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

clawMastery :: CardDef
clawMastery = CardDef
  { cdCardCode = "43005"
  , cdName = "Claw Mastery"
  , cdCost = Just 1
  , cdTraits = fromList [AnTrait.Skill]
  , cdKeywords = fromList []
  , cdCardType = AnCard.EventType
  , cdAbilityType = Just AnAbility.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = [(AnCard.PrintedResource, AnResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Just (AnCard.MaxPerRound 1)
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

regenerativeLongevity :: CardDef
regenerativeLongevity = CardDef
  { cdCardCode = "43006"
  , cdName = "Regenerative Longevity"
  , cdCost = Just 1
  , cdTraits = fromList [AnTrait.Superpower]
  , cdKeywords = fromList []
  , cdCardType = AnCard.EventType
  , cdAbilityType = Just AnAbility.Action
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.AnyCriteria [AnCriteria.CharacterExists (AnMatch.IdentityCharacters AnMatch.You <> AnMatch.CharacterWithAnyDamage <> AnMatch.CharacterCanBeHealed), AnCriteria.CharacterExists (AnMatch.AllyCharacters (AnMatch.AllyWithTitle "Honey Badger") <> AnMatch.CharacterWithAnyDamage <> AnMatch.CharacterCanBeHealed)]
  , cdResources = [(AnCard.PrintedResource, AnResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

sisterlyBond :: CardDef
sisterlyBond = CardDef
  { cdCardCode = "43007"
  , cdName = "Sisterly Bond"
  , cdCost = Just 0
  , cdTraits = fromList [AnTrait.Condition]
  , cdKeywords = fromList []
  , cdCardType = AnCard.EventType
  , cdAbilityType = Just AnAbility.HeroInterrupt
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = [(AnCard.PrintedResource, AnResource.Wild)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (AnWindow.WindowOneOf [AnWindow.AllyAttacked AnWindow.When (AnMatch.AllyWithTitle "Honey Badger") AnMatch.AnyEnemy, AnWindow.AllyThwarted AnWindow.When (AnMatch.AllyWithTitle "Honey Badger") AnMatch.AnyScheme])
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

criticalHit :: CardDef
criticalHit = CardDef
  { cdCardCode = "43016"
  , cdName = "Critical Hit"
  , cdCost = Just 2
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AnCard.EventType
  , cdAbilityType = Just AnAbility.HeroResponse
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just AnAspect.Aggression
  , cdCriteria = AnCriteria.VictoryDisplayHasSideScheme
  , cdResources = [(AnCard.PrintedResource, AnResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (AnWindow.IdentityAttackResolved AnMatch.You)
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

predictablePloy :: CardDef
predictablePloy = CardDef
  { cdCardCode = "43038"
  , cdName = "Predictable Ploy"
  , cdCost = Just 2
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AnCard.EventType
  , cdAbilityType = Just AnAbility.HeroInterrupt
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just AnAspect.Justice
  , cdCriteria = AnCriteria.VictoryDisplayHasSideScheme
  , cdResources = [(AnCard.PrintedResource, AnResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (AnWindow.TreacheryRevealed AnWindow.When AnMatch.AnyTreachery AnWindow.RevealedFromEncounterDeck)
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

anticipatedAttack :: CardDef
anticipatedAttack = CardDef
  { cdCardCode = "43040"
  , cdName = "Anticipated Attack"
  , cdCost = Just 2
  , cdTraits = fromList [AnTrait.Defense]
  , cdKeywords = fromList []
  , cdCardType = AnCard.EventType
  , cdAbilityType = Just AnAbility.HeroInterrupt
  , cdAbilitySubTypes = [AnAbility.Defense]
  , cdUnique = False
  , cdAspect = Just AnAspect.Protection
  , cdCriteria = AnCriteria.VictoryDisplayHasSideScheme
  , cdResources = [(AnCard.PrintedResource, AnResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (AnWindow.EnemyAttacksPlayerOrAlly AnWindow.When AnMatch.AnyEnemy AnMatch.AnyIdentity)
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

-- Deadpool

exhaustingPersonality :: CardDef
exhaustingPersonality = DpCard.CardDef
  { cdCardCode = "44003"
  , cdName = DpName.Name "Exhausting Personality" (Nothing)
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.EventType
  , cdAbilityType = Just DpAbility.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = [(DpCard.PrintedResource, DpResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

maximumEffort :: CardDef
maximumEffort = DpCard.CardDef
  { cdCardCode = "44004"
  , cdName = DpName.Name "Maximum Effort" (Nothing)
  , cdCost = Just 0
  , cdTraits = fromList [DpTrait.Attack]
  , cdKeywords = fromList []
  , cdCardType = DpCard.EventType
  , cdAbilityType = Just DpAbility.HeroAction
  , cdAbilitySubTypes = [DpAbility.Attack]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = DpCriteria.EnemyExists DpMatch.AttackableEnemy
  , cdResources = [(DpCard.PrintedResource, DpResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.UpToRemainingHitPointsDamageCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

metaknowledge :: CardDef
metaknowledge = DpCard.CardDef
  { cdCardCode = "44005"
  , cdName = DpName.Name "Metaknowledge" (Nothing)
  , cdCost = Just 1
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.EventType
  , cdAbilityType = Just DpAbility.HeroInterrupt
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = DpCriteria.CanCancelRevealedEncounterCard
  , cdResources = [(DpCard.PrintedResource, DpResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (DpWindow.EncounterCardRevealedBy DpWindow.When DpMatch.AnyIdentity DpMatch.AnyEncounterCard)
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

yooHoo :: CardDef
yooHoo = DpCard.CardDef
  { cdCardCode = "44006"
  , cdName = DpName.Name "\"Yoo-Hoo!\"" (Nothing)
  , cdCost = Just 1
  , cdTraits = fromList [DpTrait.Thwart]
  , cdKeywords = fromList []
  , cdCardType = DpCard.EventType
  , cdAbilityType = Just DpAbility.HeroAction
  , cdAbilitySubTypes = [DpAbility.Thwart]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = DpCriteria.SchemeExists DpMatch.ThwartableScheme
  , cdResources = [(DpCard.PrintedResource, DpResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.UpToRemainingHitPointsDamageCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

thisCardIsFire :: CardDef
thisCardIsFire = DpCard.CardDef
  { cdCardCode = "44012"
  , cdName = DpName.Name "This Card is Fire" (Nothing)
  , cdCost = Just 3
  , cdTraits = fromList [DpTrait.Attack]
  , cdKeywords = fromList []
  , cdCardType = DpCard.EventType
  , cdAbilityType = Just DpAbility.HeroAction
  , cdAbilitySubTypes = [DpAbility.Attack]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = DpCriteria.EnemyExists DpMatch.AttackableEnemy
  , cdResources = [(DpCard.PrintedResource, DpResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

barelyAScratch :: CardDef
barelyAScratch = DpCard.CardDef
  { cdCardCode = "44017"
  , cdName = DpName.Name "Barely a Scratch" (Nothing)
  , cdCost = Just 0
  , cdTraits = fromList [DpTrait.Defense]
  , cdKeywords = fromList []
  , cdCardType = DpCard.EventType
  , cdAbilityType = Just DpAbility.HeroInterrupt
  , cdAbilitySubTypes = [DpAbility.Defense]
  , cdUnique = False
  , cdAspect = Just DpAspect.Pool
  , cdCriteria = DpCriteria.EncounterIconExists
  , cdResources = [(DpCard.PrintedResource, DpResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (DpWindow.WouldTakeDamage DpMatch.You DpDamage.FromAttack DpMatch.AnyValue)
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

cutupper :: CardDef
cutupper = DpCard.CardDef
  { cdCardCode = "44018"
  , cdName = DpName.Name "Cutupper" (Nothing)
  , cdCost = Just 3
  , cdTraits = fromList [DpTrait.Attack]
  , cdKeywords = fromList []
  , cdCardType = DpCard.EventType
  , cdAbilityType = Just DpAbility.HeroAction
  , cdAbilitySubTypes = [DpAbility.Attack]
  , cdUnique = False
  , cdAspect = Just DpAspect.Pool
  , cdCriteria = DpCriteria.EnemyExists DpMatch.AttackableEnemy
  , cdResources = [(DpCard.PrintedResource, DpResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

daBomb :: CardDef
daBomb = DpCard.CardDef
  { cdCardCode = "44019"
  , cdName = DpName.Name "Da Bomb" (Nothing)
  , cdCost = Just 6
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.EventType
  , cdAbilityType = Just DpAbility.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just DpAspect.Pool
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = [(DpCard.PrintedResource, DpResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

getRageY :: CardDef
getRageY = DpCard.CardDef
  { cdCardCode = "44020"
  , cdName = DpName.Name "Get Rage-y" (Nothing)
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.EventType
  , cdAbilityType = Just DpAbility.Action
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just DpAspect.Pool
  , cdCriteria = DpCriteria.AllyExists DpMatch.AnyAlly
  , cdResources = [(DpCard.PrintedResource, DpResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

iGotThis :: CardDef
iGotThis = DpCard.CardDef
  { cdCardCode = "44021"
  , cdName = DpName.Name "\"I Got This\"" (Nothing)
  , cdCost = Just 1
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.EventType
  , cdAbilityType = Just DpAbility.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just DpAspect.Pool
  , cdCriteria = DpCriteria.PoolIconEffectsCanResolve
  , cdResources = [(DpCard.PrintedResource, DpResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

notMyResponsibility :: CardDef
notMyResponsibility = DpCard.CardDef
  { cdCardCode = "44022"
  , cdName = DpName.Name "Not my Responsibility" (Nothing)
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.EventType
  , cdAbilityType = Just DpAbility.Interrupt
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just DpAspect.Pool
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = [(DpCard.PrintedResource, DpResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (DpWindow.ThreatWouldBePlaced DpWindow.AnyThreatSource DpMatch.AnyScheme)
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

poolInspection :: CardDef
poolInspection = DpCard.CardDef
  { cdCardCode = "44023"
  , cdName = DpName.Name "'Pool Inspection" (Nothing)
  , cdCost = Just 6
  , cdTraits = fromList [DpTrait.Thwart]
  , cdKeywords = fromList []
  , cdCardType = DpCard.EventType
  , cdAbilityType = Just DpAbility.HeroAction
  , cdAbilitySubTypes = [DpAbility.Thwart]
  , cdUnique = False
  , cdAspect = Just DpAspect.Pool
  , cdCriteria = DpCriteria.SchemeExists DpMatch.ThwartableSchemeIgnoringCrisis
  , cdResources = [(DpCard.PrintedResource, DpResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

breakTime :: CardDef
breakTime = DpCard.CardDef
  { cdCardCode = "44046"
  , cdName = DpName.Name "Break Time" (Nothing)
  , cdCost = Just 3
  , cdTraits = fromList []
  , cdKeywords = fromList [DpKeyword.Alliance, DpKeyword.PerPlayerResourceCost 3]
  , cdCardType = DpCard.EventType
  , cdAbilityType = Just DpAbility.AlterEgoAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just DpAspect.Pool
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = [(DpCard.PrintedResource, DpResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

getInFrontOfMe :: CardDef
getInFrontOfMe = DpCard.CardDef
  { cdCardCode = "44047"
  , cdName = DpName.Name "Get in Front of Me!" (Nothing)
  , cdCost = Just 1
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.EventType
  , cdAbilityType = Just DpAbility.HeroInterrupt
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just DpAspect.Pool
  , cdCriteria = DpCriteria.CanCancelRevealedTreachery
  , cdResources = [(DpCard.PrintedResource, DpResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (DpWindow.TreacheryRevealed DpWindow.When DpMatch.AnyTreachery DpWindow.RevealedFromEncounterDeck)
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

mulligan :: CardDef
mulligan = DpCard.CardDef
  { cdCardCode = "44048"
  , cdName = DpName.Name "Mulligan" (Nothing)
  , cdCost = Just 3
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.EventType
  , cdAbilityType = Just DpAbility.Action
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just DpAspect.Pool
  , cdCriteria = DpCriteria.NoCardsPlayedThisPhase
  , cdResources = [(DpCard.PrintedResource, DpResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Bishop

concussiveBlast :: CardDef
concussiveBlast = CardDef
  { cdCardCode = "45007"
  , cdName = "Concussive Blast"
  , cdCost = Just 3
  , cdTraits = fromList [BpTrait.Attack, BpTrait.Superpower]
  , cdKeywords = fromList []
  , cdCardType = BpCard.EventType
  , cdAbilityType = Just BpAbility.HeroAction
  , cdAbilitySubTypes = [BpAbility.Attack]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = canAttack
  , cdResources = [(BpCard.PrintedResource, BpResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = BpCost.NoCost
  }

commandAuthority :: CardDef
commandAuthority = CardDef
  { cdCardCode = "45008"
  , cdName = "Command Authority"
  , cdCost = Just 2
  , cdTraits = fromList [BpTrait.Thwart]
  , cdKeywords = fromList []
  , cdCardType = BpCard.EventType
  , cdAbilityType = Just BpAbility.HeroAction
  , cdAbilitySubTypes = [BpAbility.Thwart]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = BpCriteria.SchemeExists BpMatch.ThwartableScheme
  , cdResources = [(BpCard.PrintedResource, BpResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = BpCost.NoCost
  }

energyConversion :: CardDef
energyConversion = CardDef
  { cdCardCode = "45009"
  , cdName = "Energy Conversion"
  , cdCost = Just 0
  , cdTraits = fromList [BpTrait.Defense]
  , cdKeywords = fromList []
  , cdCardType = BpCard.EventType
  , cdAbilityType = Just BpAbility.HeroInterrupt
  , cdAbilitySubTypes = [BpAbility.Defense]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = BpCriteria.NoCriteria
  , cdResources = [(BpCard.PrintedResource, BpResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (BpWindow.EnemyAttacksPlayerOrAlly BpWindow.When BpMatch.AnyEnemy BpMatch.AnyIdentity)
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = BpCost.NoCost
  }

sideBySide :: CardDef
sideBySide = CardDef
  { cdCardCode = "45016"
  , cdName = "Side-by-Side"
  , cdCost = Just 2
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = BpCard.EventType
  , cdAbilityType = Just BpAbility.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just BpAspect.Leadership
  , cdCriteria = BpCriteria.NoCriteria
  , cdResources = [(BpCard.PrintedResource, BpResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = BpCost.ReadyAllyCost (BpMatch.AllyWithUpgrade (BpMatch.UpgradeIs Upgrades.sidekick))
  }

suitUp :: CardDef
suitUp = CardDef
  { cdCardCode = "45017"
  , cdName = "Suit Up"
  , cdCost = Just 2
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = BpCard.EventType
  , cdAbilityType = Just BpAbility.AlterEgoAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just BpAspect.Leadership
  , cdCriteria = BpCriteria.NoCriteria
  , cdResources = [(BpCard.PrintedResource, BpResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = BpCost.NoCost
  }

-- Age of Apocalypse: Magik
scrying :: CardDef
scrying = CardDef
  { cdCardCode = "45036"
  , cdName = "Scrying"
  , cdCost = Just 0
  , cdTraits = fromList [AoaMT.Spell]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just AoaMAbility.Action
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, AoaMR.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

-- Age of Apocalypse: Magik
steppingDisc :: CardDef
steppingDisc = CardDef
  { cdCardCode = "45037"
  , cdName = "Stepping Disc"
  , cdCost = Just 1
  , cdTraits = fromList [AoaMT.Superpower]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just AoaMAbility.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, AoaMR.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

-- Age of Apocalypse: Magik
exorcism :: CardDef
exorcism = CardDef
  { cdCardCode = "45038"
  , cdName = "Exorcism"
  , cdCost = Just 2
  , cdTraits = fromList [AoaMT.Spell, AoaMT.Thwart]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just AoaMAbility.HeroAction
  , cdAbilitySubTypes = [AoaMAbility.Thwart]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = SchemeExists AoaMM.ThwartableScheme
  , cdResources = [(PrintedResource, AoaMR.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

-- Age of Apocalypse: Magik
soulStrike :: CardDef
soulStrike = CardDef
  { cdCardCode = "45039"
  , cdName = "Soul Strike"
  , cdCost = Just 2
  , cdTraits = fromList [AoaMT.Attack, AoaMT.Spell]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just AoaMAbility.HeroAction
  , cdAbilitySubTypes = [AoaMAbility.Attack]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = canAttack
  , cdResources = [(PrintedResource, AoaMR.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

-- Age of Apocalypse: Magik
magicBarrier :: CardDef
magicBarrier = CardDef
  { cdCardCode = "45040"
  , cdName = "Magic Barrier"
  , cdCost = Just 1
  , cdTraits = fromList [AoaMT.Defense, AoaMT.Spell]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just AoaMAbility.HeroInterrupt
  , cdAbilitySubTypes = [AoaMAbility.Defense]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, AoaMR.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (AoaMW.EnemyAttacksPlayerOrAlly AoaMW.When AoaMM.AnyEnemy AoaMM.AnyIdentity)
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

-- Age of Apocalypse: Magik
fullBodyCharge :: CardDef
fullBodyCharge = CardDef
  { cdCardCode = "45045"
  , cdName = "Full-Body Charge"
  , cdCost = Just 4
  , cdTraits = fromList [AoaMT.Attack]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just AoaMAbility.HeroAction
  , cdAbilitySubTypes = [AoaMAbility.Attack]
  , cdUnique = False
  , cdAspect = Just AoaMA.Aggression
  , cdCriteria = canAttack
  , cdResources = [(PrintedResource, AoaMR.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

-- Age of Apocalypse: Magik
basicSpell :: CardDef
basicSpell = CardDef
  { cdCardCode = "45051"
  , cdName = "Basic Spell"
  , cdCost = Just 2
  , cdTraits = fromList [AoaMT.Spell]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just AoaMAbility.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just AoaMA.Neutral
  , cdCriteria = SelfMatches (AoaMM.IdentityWithTrait AoaMT.Mystic)
  , cdResources = [(PrintedResource, AoaMR.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

-- Magneto

electromagneticBlast :: CardDef
electromagneticBlast = CardDef
  { cdCardCode = "49008"
  , cdName = "Electromagnetic Blast"
  , cdCost = Just 2
  , cdTraits = fromList [MgTrait.Magnetic, MgTrait.Superpower, MgTrait.Thwart]
  , cdKeywords = fromList []
  , cdCardType = MgCard.EventType
  , cdAbilityType = Just MgAbility.HeroAction
  , cdAbilitySubTypes = [MgAbility.Thwart]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = MgCriteria.SchemeExists MgMatch.ThwartableScheme
  , cdResources = [(MgCard.PrintedResource, MgResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.NoCost
  }

metalShards :: CardDef
metalShards = CardDef
  { cdCardCode = "49009"
  , cdName = "Metal Shards"
  , cdCost = Just 3
  , cdTraits = fromList [MgTrait.Attack, MgTrait.Magnetic, MgTrait.Superpower]
  , cdKeywords = fromList []
  , cdCardType = MgCard.EventType
  , cdAbilityType = Just MgAbility.HeroAction
  , cdAbilitySubTypes = [MgAbility.Attack]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = canAttack
  , cdResources = [(MgCard.PrintedResource, MgResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.NoCost
  }

magneticMissile :: CardDef
magneticMissile = CardDef
  { cdCardCode = "49010"
  , cdName = "Magnetic Missile"
  , cdCost = Just 1
  , cdTraits = fromList [MgTrait.Magnetic, MgTrait.Superpower]
  , cdKeywords = fromList []
  , cdCardType = MgCard.EventType
  , cdAbilityType = Just MgAbility.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = MgCriteria.UpgradeExists (MgMatch.UpgradeIs Upgrade.wrappedInMetal)
  , cdResources = [(MgCard.PrintedResource, MgResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.NoCost
  }

squaredOff :: CardDef
squaredOff = CardDef
  { cdCardCode = "49017"
  , cdName = "Squared Off"
  , cdCost = Just 0
  , cdTraits = fromList [MgTrait.Tactic]
  , cdKeywords = fromList []
  , cdCardType = MgCard.EventType
  , cdAbilityType = Just MgAbility.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just MgAspect.Leadership
  , cdCriteria = MgCriteria.ExtendedCardExists (MgMatch.InHandOf MgMatch.You (MgMatch.BasicCardMatches (MgCard.CardWithType MgCard.AllyType)))
  , cdResources = [(MgCard.PrintedResource, MgResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.DiscardEncounterCardsUntilMinionCost
  }

nobleSacrifice :: CardDef
nobleSacrifice = CardDef
  { cdCardCode = "49018"
  , cdName = "Noble Sacrifice"
  , cdCost = Just 1
  , cdTraits = fromList [MgTrait.Tactic]
  , cdKeywords = fromList []
  , cdCardType = MgCard.EventType
  , cdAbilityType = Just MgAbility.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just MgAspect.Leadership
  , cdCriteria = MgCriteria.NoCriteria
  , cdResources = [(MgCard.PrintedResource, MgResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.DiscardAllyCost MgMatch.AnyAlly
  }

youGotThis :: CardDef
youGotThis = CardDef
  { cdCardCode = "49019"
  , cdName = "\"You Got This!\""
  , cdCost = Just 1
  , cdTraits = fromList [MgTrait.Tactic]
  , cdKeywords = fromList []
  , cdCardType = MgCard.EventType
  , cdAbilityType = Just MgAbility.HeroResponse
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just MgAspect.Leadership
  , cdCriteria = MgCriteria.NoCriteria
  , cdResources = [(MgCard.PrintedResource, MgResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just (MgWindow.IdentityMakesBasicAttackOrThwart MgWindow.When MgMatch.You)
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.DiscardAllyCost MgMatch.AnyAlly
  }

faceThePast :: CardDef
faceThePast = CardDef
  { cdCardCode = "49022"
  , cdName = "Face the Past"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MgCard.EventType
  , cdAbilityType = Just MgAbility.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just MgAspect.Neutral
  , cdCriteria = MgCriteria.NemesisMinionOutOfPlay
  , cdResources = [(MgCard.PrintedResource, MgResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.RevealNemesisMinionCost
  }

-- Iceman
arcticAttack :: CardDef
arcticAttack = IcemanPack.arcticAttack

-- Iceman
iceBlast :: CardDef
iceBlast = IcemanPack.iceBlast

-- Iceman
chillOut :: CardDef
chillOut = IcemanPack.chillOut

-- Iceman
surpriseMove :: CardDef
surpriseMove = IcemanPack.surpriseMove

-- Iceman
takeThat :: CardDef
takeThat = IcemanPack.takeThat

-- Nightcrawler
nc48007 :: CardDef
nc48007 = CardDef
  { cdCardCode = "48007"
  , cdName = "'Port and Punch"
  , cdCost = Just 2
  , cdTraits = fromList [NcTrait.Attack, NcTrait.Superpower]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just NcAbility.HeroAction
  , cdAbilitySubTypes = [NcAbility.Attack]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NcCriteria.canAttack
  , cdResources = [(PrintedResource, NcResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NcCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Nightcrawler
nc48008 :: CardDef
nc48008 = CardDef
  { cdCardCode = "48008"
  , cdName = "Teleport Drop"
  , cdCost = Just 2
  , cdTraits = fromList [NcTrait.Attack, NcTrait.Superpower]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just NcAbility.HeroAction
  , cdAbilitySubTypes = [NcAbility.Attack]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NcCriteria.canAttack
  , cdResources = [(PrintedResource, NcResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NcCost.DiscardEnemyUpgradeCost (NcCard.CardWithTitle "Bamf!") NcMatch.AttackableEnemy
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Nightcrawler
nc48009 :: CardDef
nc48009 = CardDef
  { cdCardCode = "48009"
  , cdName = "Scout Ahead"
  , cdCost = Just 1
  , cdTraits = fromList [NcTrait.Superpower, NcTrait.Thwart]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just NcAbility.HeroAction
  , cdAbilitySubTypes = [NcAbility.Thwart]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NcCriteria.SelfMatches NcMatch.IdentityCanThwart
  , cdResources = [(PrintedResource, NcResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NcCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Nightcrawler
nc48010 :: CardDef
nc48010 = CardDef
  { cdCardCode = "48010"
  , cdName = "'Port Away"
  , cdCost = Just 0
  , cdTraits = fromList [NcTrait.Superpower]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just NcAbility.Action
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NcCriteria.NoCriteria
  , cdResources = [(PrintedResource, NcResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NcCost.DiscardMatchingHandCardCost (NcCard.CardWithTitle "Bamf!")
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Nightcrawler
nc48011 :: CardDef
nc48011 = CardDef
  { cdCardCode = "48011"
  , cdName = "Tally Ho!"
  , cdCost = Just 1
  , cdTraits = fromList [NcTrait.Defense, NcTrait.Superpower]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just NcAbility.HeroResponse
  , cdAbilitySubTypes = [NcAbility.Defense]
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NcCriteria.NoCriteria
  , cdResources = [(PrintedResource, NcResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just $ NcWindow.BamfDefense NcWindow.After NcMatch.You
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NcCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Nightcrawler
nc48016 :: CardDef
nc48016 = CardDef
  { cdCardCode = "48016"
  , cdName = "\"Come Get Me, Bub!\""
  , cdCost = Just 0
  , cdTraits = fromList [NcTrait.Tactic]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just NcAbility.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just NcAspect.Protection
  , cdCriteria = NcCriteria.NoCriteria
  , cdResources = [(PrintedResource, NcResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NcCost.DiscardEncounterCardsUntilMinionCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Nightcrawler
nc48018 :: CardDef
nc48018 = CardDef
  { cdCardCode = "48018"
  , cdName = "Riposte"
  , cdCost = Just 1
  , cdTraits = fromList [NcTrait.Defense]
  , cdKeywords = fromList []
  , cdCardType = EventType
  , cdAbilityType = Just NcAbility.HeroInterrupt
  , cdAbilitySubTypes = [NcAbility.Defense]
  , cdUnique = False
  , cdAspect = Just NcAspect.Protection
  , cdCriteria = NcCriteria.NoCriteria
  , cdResources = [(PrintedResource, NcResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Just $ NcWindow.HeroDefended NcWindow.When NcMatch.You NcMatch.AnyEnemy
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NcCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Nightcrawler
nc48031 :: CardDef
nc48031 = CardDef
  { cdCardCode = "48031"
  , cdName = "Combine Forces"
  , cdCost = Just 1
  , cdTraits = fromList []
  , cdKeywords = fromList [NcKeyword.Alliance]
  , cdCardType = EventType
  , cdAbilityType = Just NcAbility.HeroAction
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just NcAspect.Aggression
  , cdCriteria = NcCriteria.MinionExists (NcMatch.NotMinion $ NcMatch.MinionWithTrait NcTrait.Elite)
  , cdResources = [(PrintedResource, NcResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NcCost.Costs [NcCost.ExhaustCharacterWithTraitCost NcTrait.XForce, NcCost.ExhaustCharacterWithTraitCost NcTrait.XMen]
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Nightcrawler
nc48032 :: CardDef
nc48032 = CardDef
  { cdCardCode = "48032"
  , cdName = "Gunboat Diplomacy"
  , cdCost = Just 1
  , cdTraits = fromList [NcTrait.Attack, NcTrait.Thwart]
  , cdKeywords = fromList [NcKeyword.Alliance]
  , cdCardType = EventType
  , cdAbilityType = Just NcAbility.HeroAction
  , cdAbilitySubTypes = [NcAbility.Attack, NcAbility.Thwart]
  , cdUnique = False
  , cdAspect = Just NcAspect.Justice
  , cdCriteria = NcCriteria.NoCriteria
  , cdResources = [(PrintedResource, NcResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NcCost.Costs [NcCost.ExhaustCharacterWithTraitCost NcTrait.XForce, NcCost.ExhaustCharacterWithTraitCost NcTrait.XMen]
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }
