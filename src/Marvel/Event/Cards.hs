module Marvel.Event.Cards where

import Marvel.Prelude

import Marvel.Keyword (Keyword (Alliance, Requirement))
import Marvel.Cost.Types

import Marvel.Ability.Types (AbilityType(..))
import Marvel.Ability.Types qualified as Ability
import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Damage
import Marvel.EncounterSet (EncounterSet (TheMarket))
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
  [ backflip
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
chaseThemDown = reprintedAs ["06013", "16041"] $
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
greatResponsibility =
  (event "01061" "Great Responsibility" 0 HeroInterrupt [] [Mental] Justice)
    { cdResponseWindow = Just (ThreatWouldBePlaced AnyThreatSource AnyScheme)
    }

getReady :: CardDef
getReady = reprintedAs ["17016"] $ (event "01069" "Get Ready" 0 Action [] [Physical] Leadership)
  { cdCriteria = AllyExists ExhaustedAlly
  }

leadFromTheFront :: CardDef
leadFromTheFront = reprintedAs ["04018"] $
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
  (event "05014" "Preemptive Strike" 1 HeroInterrupt [Defense] [Energy] Protection)
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
concussiveBlow =
  (event "05031" "Concussive Blow" 3 HeroAction [Attack] [Physical] Justice)
    { cdAbilitySubTypes = [Ability.Attack]
    }

moraleBoost :: CardDef
moraleBoost = event "05032" "Morale Boost" 1 HeroAction [] [Energy] Leadership
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
toeToToe =
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
stealthStrike = (event "08013" "Stealth Strike" 3 HeroAction [Attack] [Mental] Justice)
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
  (event "06015" "Mean Swing" 0 HeroInterrupt [Skill] [Physical] Aggression)
    { cdResponseWindow = Just (MakesBasicAttack When You)
    , cdCriteria = SelfMatches You
    , cdAbilityCost = ExhaustUpgradeCost (UpgradeWithTrait Weapon)
    }

invulnerability :: CardDef
invulnerability = basicEvent "06021" "Invulnerability" 3 HeroAction [Superpower] [Physical]

-- | "Exhaust an ally you control" is part of its cost.
teamwork :: CardDef
teamwork =
  (event "06032" "Teamwork" 0 HeroInterrupt [Tactic] [Energy] Leadership)
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
momentOfTriumph =
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
perseverance =
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
swiftRetribution =
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
spiritualMeditation =
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
baitAndSwitch =
  reprintedAs ["27013"] $ (event "15030" "Bait and Switch" 1 HeroAction [Thwart] [Energy] Protection)
    { cdAbilitySubTypes = [Ability.Thwart]
    }

recuperation :: CardDef
recuperation =
  (basicEvent "15031" "Recuperation" 2 AlterEgoAction [] [Physical])
    { cdCriteria = SelfMatches IdentityWithAnyDamage
    }

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
everVigilant =
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
clobber =
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
hitAndRun =
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
  (event "16043" "Looking for Trouble" 0 HeroAction [Thwart] [Physical] Aggression)
    { cdAbilitySubTypes = [Ability.Thwart]
    , cdAbilityCost = DiscardEncounterCardsUntilMinionCost
    }

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
makingAnEntrance =
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
oneWayOrAnother = deckLimited 3 $ (event "22015" "One Way or Another" 0 HeroAction [] [Mental] Justice)
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
