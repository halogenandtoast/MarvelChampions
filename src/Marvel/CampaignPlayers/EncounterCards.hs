-- | Obligations and nemesis cards accompanying the Civil War and Fear No Evil heroes.
module Marvel.CampaignPlayers.EncounterCards where

import Marvel.Prelude
import Marvel.Card.Def
import Marvel.Boost
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.EncounterSet
import Marvel.Keyword qualified as K
import Marvel.Trait.Types qualified as T

nemesisCard :: CardDef
nemesisCard = CardDef
  { cdCardCode = "56023", cdName = "In Too Deep", cdCost = Nothing
  , cdTraits = mempty, cdKeywords = mempty, cdCardType = ObligationType
  , cdAbilityType = Nothing, cdAbilitySubTypes = [], cdUnique = False
  , cdAspect = Nothing, cdCriteria = NoCriteria, cdResources = []
  , cdBoostIcons = [], cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Nothing, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False
  , cdDoubleWildResources = False, cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

enc56023 :: CardDef
enc56023 = nemesisCard
  { cdCardCode = "56023", cdName = "In Too Deep"
  , cdCardType = ObligationType, cdTraits = fromList []
  , cdUnique = False, cdBoostIcons = [Boost, Boost]
  , cdKeywords = fromList []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  }

enc56024 :: CardDef
enc56024 = nemesisCard
  { cdCardCode = "56024", cdName = "The Hood"
  , cdCardType = MinionType, cdTraits = fromList [T.Criminal]
  , cdUnique = True, cdBoostIcons = [Boost, Boost]
  , cdKeywords = fromList [K.Steady, K.Villainous]
  , cdEncounterSet = Just TigraNemesis, cdEncounterSetQuantity = Just 1
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  }

enc56025 :: CardDef
enc56025 = nemesisCard
  { cdCardCode = "56025", cdName = "Criminal Underworld"
  , cdCardType = SideSchemeType, cdTraits = fromList []
  , cdUnique = False, cdBoostIcons = [Boost, Boost, Boost]
  , cdKeywords = fromList []
  , cdEncounterSet = Just TigraNemesis, cdEncounterSetQuantity = Just 1
  , cdHazards = 0, cdAcceleration = 1, cdAmplify = 0
  }

enc56026 :: CardDef
enc56026 = nemesisCard
  { cdCardCode = "56026", cdName = "The Hood's Mantle"
  , cdCardType = AttachmentType, cdTraits = fromList [T.Armor]
  , cdUnique = True, cdBoostIcons = [Star]
  , cdKeywords = fromList []
  , cdEncounterSet = Just TigraNemesis, cdEncounterSetQuantity = Just 1
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  }

enc56027 :: CardDef
enc56027 = nemesisCard
  { cdCardCode = "56027", cdName = "Madame Masque"
  , cdCardType = MinionType, cdTraits = fromList [T.Criminal]
  , cdUnique = True, cdBoostIcons = [Star]
  , cdKeywords = fromList []
  , cdEncounterSet = Just TigraNemesis, cdEncounterSetQuantity = Just 1
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  }

enc56028 :: CardDef
enc56028 = nemesisCard
  { cdCardCode = "56028", cdName = "Jigsaw"
  , cdCardType = MinionType, cdTraits = fromList [T.Criminal]
  , cdUnique = True, cdBoostIcons = [Boost, Boost]
  , cdKeywords = fromList [K.Quickstrike]
  , cdEncounterSet = Just TigraNemesis, cdEncounterSetQuantity = Just 1
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  }

enc56054 :: CardDef
enc56054 = nemesisCard
  { cdCardCode = "56054", cdName = "Complicated Lineage"
  , cdCardType = ObligationType, cdTraits = fromList []
  , cdUnique = False, cdBoostIcons = [Boost, Boost]
  , cdKeywords = fromList []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  }

enc56055 :: CardDef
enc56055 = nemesisCard
  { cdCardCode = "56055", cdName = "Super Skrull"
  , cdCardType = MinionType, cdTraits = fromList [T.Elite, T.Skrull]
  , cdUnique = True, cdBoostIcons = [Boost, Boost]
  , cdKeywords = fromList []
  , cdEncounterSet = Just HulklingNemesis, cdEncounterSetQuantity = Just 1
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  }

enc56056 :: CardDef
enc56056 = nemesisCard
  { cdCardCode = "56056", cdName = "Skrull Business"
  , cdCardType = SideSchemeType, cdTraits = fromList []
  , cdUnique = False, cdBoostIcons = [Boost, Boost, Boost]
  , cdKeywords = fromList []
  , cdEncounterSet = Just HulklingNemesis, cdEncounterSetQuantity = Just 1
  , cdHazards = 0, cdAcceleration = 1, cdAmplify = 0
  }

enc56057 :: CardDef
enc56057 = nemesisCard
  { cdCardCode = "56057", cdName = "Fantastic Powers"
  , cdCardType = AttachmentType, cdTraits = fromList [T.Superpower]
  , cdUnique = False, cdBoostIcons = [Boost, Boost]
  , cdKeywords = fromList []
  , cdEncounterSet = Just HulklingNemesis, cdEncounterSetQuantity = Just 1
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  }

enc56058 :: CardDef
enc56058 = nemesisCard
  { cdCardCode = "56058", cdName = "You're Coming With Me!"
  , cdCardType = TreacheryType, cdTraits = fromList []
  , cdUnique = False, cdBoostIcons = [Boost]
  , cdKeywords = fromList []
  , cdEncounterSet = Just HulklingNemesis, cdEncounterSetQuantity = Just 2
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  }

enc60032 :: CardDef
enc60032 = nemesisCard
  { cdCardCode = "60032", cdName = "Sensory Overload"
  , cdCardType = ObligationType, cdTraits = fromList []
  , cdUnique = False, cdBoostIcons = [Boost, Boost]
  , cdKeywords = fromList []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  }

enc60033 :: CardDef
enc60033 = nemesisCard
  { cdCardCode = "60033", cdName = "Bullseye"
  , cdCardType = MinionType, cdTraits = fromList [T.Assassin]
  , cdUnique = True, cdBoostIcons = [Boost, Boost, Boost]
  , cdKeywords = fromList []
  , cdEncounterSet = Just DaredevilNemesis, cdEncounterSetQuantity = Just 1
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  }

enc60034 :: CardDef
enc60034 = nemesisCard
  { cdCardCode = "60034", cdName = "Deadliest Man Alive"
  , cdCardType = SideSchemeType, cdTraits = fromList []
  , cdUnique = False, cdBoostIcons = [Boost, Boost]
  , cdKeywords = fromList []
  , cdEncounterSet = Just DaredevilNemesis, cdEncounterSetQuantity = Just 1
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 1
  }

enc60035 :: CardDef
enc60035 = nemesisCard
  { cdCardCode = "60035", cdName = "Stolen Sai"
  , cdCardType = AttachmentType, cdTraits = fromList [T.Weapon]
  , cdUnique = False, cdBoostIcons = [Star, Boost]
  , cdKeywords = fromList []
  , cdEncounterSet = Just DaredevilNemesis, cdEncounterSetQuantity = Just 1
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  }

enc60036 :: CardDef
enc60036 = nemesisCard
  { cdCardCode = "60036", cdName = "Eye on the Target"
  , cdCardType = TreacheryType, cdTraits = fromList []
  , cdUnique = False, cdBoostIcons = [Boost, Boost]
  , cdKeywords = fromList []
  , cdEncounterSet = Just DaredevilNemesis, cdEncounterSetQuantity = Just 2
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  }

enc60060 :: CardDef
enc60060 = nemesisCard
  { cdCardCode = "60060", cdName = "Raised by the Kingpin"
  , cdCardType = ObligationType, cdTraits = fromList []
  , cdUnique = False, cdBoostIcons = [Boost, Boost]
  , cdKeywords = fromList []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  }

enc60061 :: CardDef
enc60061 = nemesisCard
  { cdCardCode = "60061", cdName = "Kingpin"
  , cdCardType = MinionType, cdTraits = fromList [T.Criminal]
  , cdUnique = True, cdBoostIcons = [Boost, Boost, Boost]
  , cdKeywords = fromList []
  , cdEncounterSet = Just EchoNemesis, cdEncounterSetQuantity = Just 1
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  }

enc60062 :: CardDef
enc60062 = nemesisCard
  { cdCardCode = "60062", cdName = "Master Manipulator"
  , cdCardType = SideSchemeType, cdTraits = fromList []
  , cdUnique = False, cdBoostIcons = [Star]
  , cdKeywords = fromList []
  , cdEncounterSet = Just EchoNemesis, cdEncounterSetQuantity = Just 1
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  }

enc60063 :: CardDef
enc60063 = nemesisCard
  { cdCardCode = "60063", cdName = "Kingpin's Henchman"
  , cdCardType = MinionType, cdTraits = fromList [T.Criminal]
  , cdUnique = False, cdBoostIcons = [Boost]
  , cdKeywords = fromList [K.Guard]
  , cdEncounterSet = Just EchoNemesis, cdEncounterSetQuantity = Just 2
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  }

enc60064 :: CardDef
enc60064 = nemesisCard
  { cdCardCode = "60064", cdName = "Pawn of the Kingpin"
  , cdCardType = TreacheryType, cdTraits = fromList []
  , cdUnique = False, cdBoostIcons = [Boost, Boost]
  , cdKeywords = fromList []
  , cdEncounterSet = Just EchoNemesis, cdEncounterSetQuantity = Just 1
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  }

