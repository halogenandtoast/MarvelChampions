module Marvel.Event.Cards where

import Marvel.Prelude

import Marvel.Ability.Types (AbilityType(..))
import Marvel.Ability.Types qualified as Ability
import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Damage
import Marvel.GameValue.Types
import Marvel.Matchers
import Marvel.Name
import Marvel.Resource
import Marvel.Trait
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
  baseEvent code name cost aType traits resources Nothing

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
  , cdAbilitySubType = Nothing
  , cdUnique = False
  , cdAspect = mAspect
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdResources = map (PrintedResource, ) resources
  , cdResponseWindow = Nothing
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdLimit = Nothing
  , cdAlternateCardCodes = []
  }

backflip :: CardDef
backflip =
  (identityEvent "01003" "Backflip" 0 Interrupt [Defense, Skill] [Physical])
    { cdResponseWindow = Just (WouldTakeDamage You FromAttack AnyValue)
    , cdAbilitySubType = Just Ability.Defense
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
  { cdAbilitySubType = Just Ability.Attack
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
  { cdAbilitySubType = Just Ability.Thwart
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
  { cdAbilitySubType = Just Ability.Attack
  }

gammaSlam :: CardDef
gammaSlam =
  (identityEvent "01021" "Gamma Slam" 4 HeroAction [Attack, Superpower] [Mental]
    )
    { cdAbilitySubType = Just Ability.Attack
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
  { cdAbilitySubType = Just Ability.Thwart
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
  { cdCriteria = EnemyExists AttackableEnemy
  , cdAbilitySubType = Just Ability.Attack
  }

supersonicPunch :: CardDef
supersonicPunch =
  (identityEvent "01032" "Supersonic Punch" 2 HeroAction [Attack] [Energy])
    { cdCriteria = EnemyExists AttackableEnemy
    , cdAbilitySubType = Just Ability.Attack
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
chaseThemDown = reprintedAs ["06013"] $
  (event "01052" "Chase Them Down" 0 Response [Thwart] [Mental] Aggression)
    { cdAbilitySubType = Just Ability.Thwart
    , cdCriteria = SchemeExists AnyScheme
    , cdResponseWindow = Just $ EnemyDefeated After AnyEnemy $ AttackFromPlayer
      You
    }

relentlessAssault :: CardDef
relentlessAssault =
  (event "01053" "Relentless Assault" 2 HeroAction [Attack] [Energy] Aggression)
    { cdAbilitySubType = Just Ability.Attack
    , cdCriteria = MinionExists AnyMinion
    }

uppercut :: CardDef
uppercut =
  (event "01054" "Uppercut" 3 HeroAction [Attack] [Physical] Aggression)
    { cdAbilitySubType = Just Ability.Attack
    }

forJustice :: CardDef
forJustice =
  (event "01060" "For Justice!" 2 HeroAction [Thwart] [Energy] Justice)
    { cdCriteria = SchemeExists ThwartableScheme
    , cdAbilitySubType = Just Ability.Thwart
    }

greatResponsibility :: CardDef
greatResponsibility =
  (event "01061" "Great Responsibility" 0 HeroInterrupt [] [Mental] Justice)
    { cdResponseWindow = Just (ThreatWouldBePlaced AnyThreatSource AnyScheme)
    }

getReady :: CardDef
getReady = (event "01069" "Get Ready" 0 Action [] [Physical] Leadership)
  { cdCriteria = AllyExists ExhaustedAlly
  }

leadFromTheFront :: CardDef
leadFromTheFront =
  event "01070" "Lead from the Front" 2 HeroAction [Tactic] [Energy] Leadership

makeTheCall :: CardDef
makeTheCall = reprintedAs ["03016"] $ (event "01071" "Make the Call" 0 Action [] [Mental] Leadership)
  { cdCriteria =
    ExtendedCardExists
    $ AffordableCardBy You
    $ InDiscardOf AnyIdentity
    $ BasicCardMatches (CardWithType AllyType)
  }

counterPunch :: CardDef
counterPunch =
  (event "01077" "Counter-Punch" 0 Response [Attack] [Physical] Protection)
    { cdResponseWindow = Just (HeroDefended After You AnyEnemy)
    , cdAbilitySubType = Just Ability.Attack
    }

getBehindMe :: CardDef
getBehindMe = reprintedAs ["05013"] $
  (event "01078" "Get Behind Me!" 1 HeroInterrupt [] [Mental] Protection)
    { cdResponseWindow =
        Just (TreacheryRevealed When AnyTreachery RevealedFromEncounterDeck)
    }

emergency :: CardDef
emergency = (basicEvent "01085" "Emergency" 0 Interrupt [Thwart] [Energy])
  { cdResponseWindow = Just (ThreatWouldBePlaced ThreatFromVillain AnyScheme)
  , cdAbilitySubType = Just Ability.Thwart
  }

firstAid :: CardDef
firstAid = (basicEvent "01086" "First Aid" 1 Action [] [Mental])
  { cdCriteria = CharacterExists CharacterWithAnyDamage
  }

haymaker :: CardDef
haymaker = (basicEvent "01087" "Haymaker" 2 HeroAction [Attack] [Energy])
  { cdAbilitySubType = Just Ability.Attack
  }

astralProjection :: CardDef
astralProjection =
  (identityEvent "09003" "Astral Projection" 2 HeroAction [Spell, Thwart] [Mental])
    { cdAbilitySubType = Just Ability.Thwart
    }

magicBlast :: CardDef
magicBlast =
  (identityEvent "09004" "Magic Blast" 3 HeroAction [Attack, Spell] [Energy])
    { cdCriteria = EnemyExists AttackableEnemy
    , cdAbilitySubType = Just Ability.Attack
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
    }

desperateDefense :: CardDef
desperateDefense =
  (event "09015" "Desperate Defense" 1 HeroInterrupt [Defense] [Energy] Protection)
    { cdResponseWindow = Just (HeroDefended When You AnyEnemy)
    , cdAbilitySubType = Just Ability.Defense
    }

-- | healing 2 damage from your hero is its cost, so it is paid only in full
momentumShift :: CardDef
momentumShift =
  (event "09016" "Momentum Shift" 2 HeroAction [Attack] [Physical] Protection)
    { cdCriteria =
        EnemyExists AttackableEnemy
          <> SelfMatches (IdentityWithDamage $ AtLeast $ Static 2)
    , cdAbilitySubType = Just Ability.Attack
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
  { cdResponseWindow = Just (BoostCardFlipped When SchemeActivation)
  }
