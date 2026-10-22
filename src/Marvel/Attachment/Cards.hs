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
      , heldHostage
      , heldHostageThunderball
      , heldHostagePiledriver
      , heldHostageBulldozer
      , magicCrowbar
      , wreckersCommand
      , ballAndChain
      , radioactiveBuildup
      , distractingTaunts
      , bulldozersHelmet
      , rammingSpeed

      -- The Rise of Red Skull

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

-- The Green Goblin

goblinGlider :: CardDef
goblinGlider = attachment "02033" "Goblin Glider" [Vehicle] [Boost, Boost, Boost] GoblinGimmicks 2

pumpkinBombs :: CardDef
pumpkinBombs = attachment "02034" "Pumpkin Bombs" [Weapon] [Boost, Boost] GoblinGimmicks 2

allTiedUp :: CardDef
allTiedUp = attachment "02048" "All Tied Up" [] [] RunningInterference 1

mediaCoverage :: CardDef
mediaCoverage = attachment "02049" "Media Coverage" [] [Boost] RunningInterference 2

-- The Wrecking Crew

heldHostage :: CardDef
heldHostage = attachment "07005" "Held Hostage" [] [Boost] Wrecker 1

-- | Held Hostage, one in each of the Wrecking Crew's decks.
heldHostageThunderball :: CardDef
heldHostageThunderball = (reprint "07021" heldHostage) {cdEncounterSet = Just Thunderball}

heldHostagePiledriver :: CardDef
heldHostagePiledriver = (reprint "07036" heldHostage) {cdEncounterSet = Just Piledriver}

heldHostageBulldozer :: CardDef
heldHostageBulldozer = (reprint "07050" heldHostage) {cdEncounterSet = Just Bulldozer}

magicCrowbar :: CardDef
magicCrowbar = unique $ attachment "07006" "Magic Crowbar" [] [Boost, Boost, Boost] Wrecker 1

wreckersCommand :: CardDef
wreckersCommand = unique $ attachment "07007" "Wrecker's Command" [] [Boost] Wrecker 1

ballAndChain :: CardDef
ballAndChain = unique $ attachment "07020" "Ball and Chain" [Weapon] [Boost, Boost, Boost] Thunderball 1

radioactiveBuildup :: CardDef
radioactiveBuildup = attachment "07022" "Radioactive Buildup" [Condition] [Boost, Boost, Boost] Thunderball 1

distractingTaunts :: CardDef
distractingTaunts = attachment "07035" "Distracting Taunts" [] [Boost, Boost] Piledriver 2

bulldozersHelmet :: CardDef
bulldozersHelmet = unique $ attachment "07049" "Bulldozer's Helmet" [Armor] [Boost, Boost, Boost] Bulldozer 1

rammingSpeed :: CardDef
rammingSpeed = attachment "07051" "Ramming Speed" [Condition] [Boost, Boost] Bulldozer 2
