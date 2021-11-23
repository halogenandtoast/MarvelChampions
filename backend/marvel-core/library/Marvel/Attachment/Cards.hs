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
allAttachments = fromList
  $ map (toCardCode &&& id) [armoredRhinoSuit, charge, enhancedIvoryHorn]

attachment
  :: CardCode
  -> Name
  -> [Trait]
  -> [BoostIcon]
  -> EncounterSet
  -> Natural
  -> CardDef
attachment code name traits boostIcons encounterSet quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = fromList traits
  , cdKeywords = mempty
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
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

armoredRhinoSuit :: CardDef
armoredRhinoSuit = attachment "01098" "Armored Rhino Suit" [Armor] [] Rhino 1

charge :: CardDef
charge = attachment "01099" "Charge" [] [Boost, Boost] Rhino 2

enhancedIvoryHorn :: CardDef
enhancedIvoryHorn =
  attachment "01100" "Enhanced Ivory Horn" [Weapon] [Boost, Boost] Rhino 1
