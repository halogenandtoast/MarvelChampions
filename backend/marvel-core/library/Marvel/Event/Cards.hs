module Marvel.Event.Cards where

import Marvel.Prelude

import Marvel.Ability.Type (AbilityType(..))
import Marvel.Ability.Type qualified as Ability
import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Name
import Marvel.Resource
import Marvel.Trait
import Marvel.Window

allEvents :: HashMap CardCode CardDef
allEvents = fromList $ map
  (toCardCode &&& id)
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
crisisInterdiction =
  (identityEvent "01012" "Crisis Interdiction" 2 HeroAction [Thwart] [Energy])
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
chaseThemDown =
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
makeTheCall = (event "01071" "Make the Call" 0 Action [] [Mental] Leadership)
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
getBehindMe =
  (event "01078" "Get Behind Me!" 1 HeroInterrupt [] [Mental] Protection)
    { cdResponseWindow = Just (HeroDefended After You AnyEnemy)
    , cdAbilitySubType = Just Ability.Attack
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
