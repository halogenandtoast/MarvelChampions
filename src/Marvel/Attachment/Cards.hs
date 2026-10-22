module Marvel.Attachment.Cards where

import Marvel.Prelude

import Marvel.Cost.Types (Cost (NoCost))

import Marvel.Boost
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet
import Data.HashSet (singleton)
import Marvel.Keyword
import Marvel.Name
import Marvel.Trait

allAttachments :: HashMap CardCode CardDef
allAttachments =
  fromList $
    map
      (toFst toCardCode)
      [ armoredRhinoSuit
      , charge
      , enhancedIvoryHorn
      , sonicConverter
      , solidSoundBody
      , programTransmitter
      , geneticallyEnhanced
      , upgradedDrones
      , vibraniumArmor
      , concussionBlasters
      , biomechanicalUpgrades
      , counterspell
      -- The Green Goblin
      , goblinGlider
      , pumpkinBombs
      , allTiedUp
      , mediaCoverage

      -- The Wrecking Crew

      -- The Rise of Red Skull
      -- Zola
      , defensiveProgramming
      , painInhibitors
      , neurologicalImplants

      -- Red Skull
      , redSkullsLuger
      , redSkullsRightHook
      , masterStrategist
      , twistedReality

      -- Taskmaster
      , taskmastersSword
      , taskmastersShield
      , photographicReflexes

      -- The Once and Future Kang

      ]

attachment ::
  CardCode ->
  Name ->
  [Trait] ->
  [BoostIcon] ->
  EncounterSet ->
  Natural ->
  CardDef
attachment code name traits boostIcons encounterSet quantity =
  CardDef
    { cdCardCode = code
    , cdName = name
    , cdCost = Nothing
    , cdTraits = fromList traits
    , cdKeywords = mempty
    , cdCardType = AttachmentType
    , cdAbilityType = Nothing
    , cdAbilitySubType = Nothing
    , cdUnique = False
    , cdAspect = Nothing
    , cdEncounterSet = Just encounterSet
    , cdEncounterSetQuantity = Just quantity
    , cdCriteria = NoCriteria
    , cdResources = []
    , cdBoostIcons = boostIcons
    , cdResponseWindow = Nothing
    , cdHazards = 0
    , cdAcceleration = 0
    , cdLimit = Nothing
    , cdAlternateCardCodes = []
    , cdPlayableFromDiscard = False
    , cdAbilityCost = NoCost
    }

unique :: CardDef -> CardDef
unique def = def {cdUnique = True}

armoredRhinoSuit :: CardDef
armoredRhinoSuit = attachment "01098" "Armored Rhino Suit" [Armor] [] Rhino 1

charge :: CardDef
charge = attachment "01099" "Charge" [Weapon] [Boost, Boost] Rhino 2

enhancedIvoryHorn :: CardDef
enhancedIvoryHorn =
  attachment "01100" "Enhanced Ivory Horn" [Weapon] [Boost, Boost] Rhino 1

sonicConverter :: CardDef
sonicConverter =
  unique $
    attachment "01118" "Sonic Converter" [Weapon] [Boost, Boost, Boost] Klaw 1

solidSoundBody :: CardDef
solidSoundBody =
  attachment "01119" "Solid-Sound Body" [Condition] [Boost, Boost, Boost] Klaw 1

programTransmitter :: CardDef
programTransmitter =
  attachment "01141" "Program Transmitter" [Item, Tech] [Boost] Ultron 1

geneticallyEnhanced :: CardDef
geneticallyEnhanced =
  attachment "01163" "Genetically Enhanced" [Condition] [Boost] SheHulkNemesis 1

upgradedDrones :: CardDef
upgradedDrones = attachment "01142" "Upgraded Drones" [Condition] [] Ultron 2

vibraniumArmor :: CardDef
vibraniumArmor = attachment "01152" "Vibranium Armor" [Armor] [Boost] UnderAttack 1

concussionBlasters :: CardDef
concussionBlasters =
  attachment "01153" "Concussion Blasters" [Weapon] [Boost] UnderAttack 1

biomechanicalUpgrades :: CardDef
biomechanicalUpgrades =
  (attachment "01185" "Biomechanical Upgrades" [Tech] [Boost] TheDoomsdayChair 3)
    { cdKeywords = singleton Surge
    }

counterspell :: CardDef
counterspell =
  attachment "09030" "Counterspell" [Condition, Spell] [Boost] DoctorStrangeNemesis 2

-- The Rise of Red Skull
-- Zola

defensiveProgramming :: CardDef
defensiveProgramming = attachment "04117" "Defensive Programming" [Tech] [Boost] Zola 3

painInhibitors :: CardDef
painInhibitors = attachment "04118" "Pain Inhibitors" [Tech] [Boost] Zola 2

neurologicalImplants :: CardDef
neurologicalImplants = attachment "04119" "Neurological Implants" [Tech] [Boost] Zola 2

-- Red Skull

redSkullsLuger :: CardDef
redSkullsLuger = unique $ attachment "04132" "Red Skull's Luger" [Weapon] [Star] RedSkull 1

redSkullsRightHook :: CardDef
redSkullsRightHook = attachment "04133" "Red Skull's Right Hook" [Skill] [Boost, Boost, Boost] RedSkull 2

masterStrategist :: CardDef
masterStrategist = attachment "04134" "Master Strategist" [Skill] [Boost, Boost] RedSkull 2

twistedReality :: CardDef
twistedReality =
  (attachment "04135" "Twisted Reality" [Condition] [Boost, Boost] RedSkull 2)
    { cdKeywords = singleton (Incite 1)
    }

-- The Green Goblin

goblinGlider :: CardDef
goblinGlider = attachment "02033" "Goblin Glider" [Vehicle] [Boost, Boost, Boost] GoblinGimmicks 2

pumpkinBombs :: CardDef
pumpkinBombs = attachment "02034" "Pumpkin Bombs" [Weapon] [Boost, Boost] GoblinGimmicks 2

allTiedUp :: CardDef
allTiedUp = attachment "02048" "All Tied Up" [] [] RunningInterference 1

mediaCoverage :: CardDef
mediaCoverage = attachment "02049" "Media Coverage" [] [Boost] RunningInterference 2

-- The Rise of Red Skull

-- Taskmaster

taskmastersSword :: CardDef
taskmastersSword = unique $ attachment "04102" "Taskmaster's Sword" [Weapon] [Boost, Boost, Boost] Taskmaster 1

taskmastersShield :: CardDef
taskmastersShield = unique $ attachment "04103" "Taskmaster's Shield" [Armor, Weapon] [Boost, Boost, Boost] Taskmaster 1

photographicReflexes :: CardDef
photographicReflexes = attachment "04104" "Photographic Reflexes" [] [Boost, Boost] Taskmaster 2
