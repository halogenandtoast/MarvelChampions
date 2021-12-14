module Marvel.Attachment.Cards where

import Marvel.Prelude

import Marvel.Boost
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet
import Marvel.Name
import Marvel.Trait

allAttachments :: HashMap CardCode CardDef
allAttachments =
  fromList $
    map
      (toCardCode &&& id)
      [ armoredRhinoSuit
      , charge
      , enhancedIvoryHorn
      , sonicConverter
      , solidSoundBody
      , geneticallyEnhanced
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
    }

unique :: CardDef -> CardDef
unique def = def {cdUnique = True}

armoredRhinoSuit :: CardDef
armoredRhinoSuit = attachment "01098" "Armored Rhino Suit" [Armor] [] Rhino 1

charge :: CardDef
charge = attachment "01099" "Charge" [] [Boost, Boost] Rhino 2

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

geneticallyEnhanced :: CardDef
geneticallyEnhanced =
  attachment "01163" "Genetically Enhanced" [Condition] [Boost] SheHulkNemesis 1
