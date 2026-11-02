{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.AgentsOfShield.EncounterCards where

import Marvel.Prelude
import Marvel.Card
import Marvel.Hood.Cards (hoodCardDef)
import Marvel.EncounterSet qualified as Set
import Marvel.Keyword qualified as K
import Marvel.Boost qualified as B
import Marvel.Trait qualified as T
import Marvel.GameValue

aos50064 :: CardDef
aos50064 = (hoodCardDef "50064" "Black Widow" Set.AosBlackWidow VillainType 0)
  { cdTraits = fromList [T.Spy]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50065 :: CardDef
aos50065 = (hoodCardDef "50065" "Black Widow" Set.AosBlackWidow VillainType 0)
  { cdTraits = fromList [T.Spy]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50066 :: CardDef
aos50066 = (hoodCardDef "50066" "Black Widow" Set.AosBlackWidow VillainType 0)
  { cdTraits = fromList [T.Spy]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50067b :: CardDef
aos50067b = (hoodCardDef "50067" "The Widow's Web" Set.AosBlackWidow MainSchemeType 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50068 :: CardDef
aos50068 = (hoodCardDef "50068" "Black Widow's Gauntlet" Set.AosBlackWidow AttachmentType 2)
  { cdTraits = fromList [T.Tech, T.Weapon]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50069 :: CardDef
aos50069 = (hoodCardDef "50069" "Grappling Hook" Set.AosBlackWidow AttachmentType 1)
  { cdTraits = fromList [T.Item]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50070 :: CardDef
aos50070 = (hoodCardDef "50070" "Night Vision Goggles" Set.AosBlackWidow AttachmentType 1)
  { cdTraits = fromList [T.Item]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50071 :: CardDef
aos50071 = (hoodCardDef "50071" "Stun Net" Set.AosBlackWidow AttachmentType 1)
  { cdTraits = fromList [T.Item]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50072 :: CardDef
aos50072 = (hoodCardDef "50072" "A.I.M. Commando" Set.AosBlackWidow MinionType 2)
  { cdTraits = fromList [T.Aim]
  , cdKeywords = fromList [K.Quickstrike]
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50073 :: CardDef
aos50073 = (hoodCardDef "50073" "A.I.M. Grunt" Set.AosBlackWidow MinionType 2)
  { cdTraits = fromList [T.Aim]
  , cdKeywords = fromList [K.Guard]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50074 :: CardDef
aos50074 = (hoodCardDef "50074" "Automated Defenses" Set.AosBlackWidow SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [K.Hinder (PerPlayer 1)]
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 1
  }

aos50075 :: CardDef
aos50075 = (hoodCardDef "50075" "Destroy Evidence" Set.AosBlackWidow SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [K.Hinder (PerPlayer 2)]
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50076 :: CardDef
aos50076 = (hoodCardDef "50076" "Attacrobatics" Set.AosBlackWidow TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50077 :: CardDef
aos50077 = (hoodCardDef "50077" "Covert Ops" Set.AosBlackWidow TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50078 :: CardDef
aos50078 = (hoodCardDef "50078" "Dance of Death" Set.AosBlackWidow TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50079 :: CardDef
aos50079 = (hoodCardDef "50079" "Widow's Bite" Set.AosBlackWidow TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50080 :: CardDef
aos50080 = (hoodCardDef "50080" "A.I.M. Abductor" Set.AosAimAbduction MinionType 2)
  { cdTraits = fromList [T.Aim]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50081 :: CardDef
aos50081 = (hoodCardDef "50081" "Abduct Superhumans" Set.AosAimAbduction SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [K.Surge]
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50082 :: CardDef
aos50082 = (hoodCardDef "50082" "Nabbed!" Set.AosAimAbduction TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50083 :: CardDef
aos50083 = (hoodCardDef "50083" "A.I.M. Scientist" Set.AosAimScience MinionType 1)
  { cdTraits = fromList [T.Aim]
  , cdKeywords = fromList [K.Surge, K.Vulnerable]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  }

aos50084 :: CardDef
aos50084 = (hoodCardDef "50084" "A.I.M. Soldier" Set.AosAimScience MinionType 3)
  { cdTraits = fromList [T.Aim]
  , cdKeywords = fromList [K.Patrol]
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50085 :: CardDef
aos50085 = (hoodCardDef "50085" "Mad Science" Set.AosAimScience SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [K.Hinder (PerPlayer 2)]
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  }

aos50086a :: CardDef
aos50086a = (hoodCardDef "50086a" "Batroc" Set.AosBatroc VillainType 0)
  { cdTraits = fromList [T.BatrocsBrigade, T.Mercenary]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50086b :: CardDef
aos50086b = (hoodCardDef "50086b" "Batroc" Set.AosBatroc VillainType 0)
  { cdTraits = fromList [T.BatrocsBrigade, T.Mercenary]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50087b :: CardDef
aos50087b = (hoodCardDef "50087" "Infiltrate A.I.M. Island Embassy" Set.AosBatroc MainSchemeType 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50088b :: CardDef
aos50088b = (hoodCardDef "50088" "Locate Missing Person" Set.AosBatroc MainSchemeType 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50089b :: CardDef
aos50089b = (hoodCardDef "50089" "Extract Captives" Set.AosBatroc MainSchemeType 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50090a :: CardDef
aos50090a = (hoodCardDef "50090a" "Alert Level" Set.AosBatroc EnvironmentType 1)
  { cdTraits = fromList [T.Low]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50090b :: CardDef
aos50090b = (hoodCardDef "50090b" "Alert Level" Set.AosBatroc EnvironmentType 0)
  { cdTraits = fromList [T.High]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50092 :: CardDef
aos50092 = (hoodCardDef "50092" "Heightened Reflexes" Set.AosBatroc AttachmentType 1)
  { cdTraits = fromList [T.Condition]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50093 :: CardDef
aos50093 = (hoodCardDef "50093" "Embassy Guard" Set.AosBatroc MinionType 2)
  { cdTraits = fromList [T.Aim]
  , cdKeywords = fromList [K.Guard, K.Vulnerable]
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50094 :: CardDef
aos50094 = (hoodCardDef "50094" "Embassy Patrol" Set.AosBatroc MinionType 2)
  { cdTraits = fromList [T.Aim]
  , cdKeywords = fromList [K.Patrol, K.Vulnerable]
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50095 :: CardDef
aos50095 = (hoodCardDef "50095" "Commandeer Security Office" Set.AosBatroc SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  }

aos50096 :: CardDef
aos50096 = (hoodCardDef "50096" "Leaping Kick" Set.AosBatroc TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50097 :: CardDef
aos50097 = (hoodCardDef "50097" "Security Cameras" Set.AosBatroc TreacheryType 4)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50098 :: CardDef
aos50098 = (hoodCardDef "50098" "Machete" Set.AosBatrocsBrigade MinionType 1)
  { cdTraits = fromList [T.BatrocsBrigade, T.Mercenary]
  , cdKeywords = fromList [K.Surge, K.Vulnerable]
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50099 :: CardDef
aos50099 = (hoodCardDef "50099" "Rapido" Set.AosBatrocsBrigade MinionType 1)
  { cdTraits = fromList [T.BatrocsBrigade, T.Mercenary]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = [B.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50100 :: CardDef
aos50100 = (hoodCardDef "50100" "Zaran" Set.AosBatrocsBrigade MinionType 1)
  { cdTraits = fromList [T.BatrocsBrigade, T.Mercenary]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50101 :: CardDef
aos50101 = (hoodCardDef "50101" "Batroc's Brigade" Set.AosBatrocsBrigade SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50102 :: CardDef
aos50102 = (hoodCardDef "50102" "Soldiers of Fortune" Set.AosBatrocsBrigade TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50103a :: CardDef
aos50103a = (hoodCardDef "50103a" "M.O.D.O.K." Set.AosModok VillainType 0)
  { cdTraits = fromList [T.Aerial, T.Cyborg, T.Psionic]
  , cdKeywords = fromList [K.Retaliate 1]
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50103b :: CardDef
aos50103b = (hoodCardDef "50103b" "M.O.D.O.K." Set.AosModok VillainType 0)
  { cdTraits = fromList [T.Aerial, T.Cyborg, T.Psionic]
  , cdKeywords = fromList [K.Steady, K.Retaliate 2]
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50104b :: CardDef
aos50104b = (hoodCardDef "50104" "Upgrading Adaptoids" Set.AosModok MainSchemeType 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50105a :: CardDef
aos50105a = (hoodCardDef "50105a" "Holding Cell" Set.AosModok EnvironmentType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50106a :: CardDef
aos50106a = (hoodCardDef "50106a" "Holding Cell" Set.AosModok EnvironmentType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50107a :: CardDef
aos50107a = (hoodCardDef "50107a" "Holding Cell" Set.AosModok EnvironmentType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50108a :: CardDef
aos50108a = (hoodCardDef "50108a" "Holding Cell" Set.AosModok EnvironmentType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50109 :: CardDef
aos50109 = (hoodCardDef "50109" "Flying Upgrade" Set.AosModok EnvironmentType 1)
  { cdTraits = fromList [T.Adaptoid]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50110 :: CardDef
aos50110 = (hoodCardDef "50110" "Psionic Upgrade" Set.AosModok EnvironmentType 1)
  { cdTraits = fromList [T.Adaptoid]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50111 :: CardDef
aos50111 = (hoodCardDef "50111" "Sarah Garza Upgrade" Set.AosModok EnvironmentType 1)
  { cdTraits = fromList [T.Adaptoid]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50112 :: CardDef
aos50112 = (hoodCardDef "50112" "Strong Upgrade" Set.AosModok EnvironmentType 1)
  { cdTraits = fromList [T.Adaptoid]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50113 :: CardDef
aos50113 = (hoodCardDef "50113" "Adaptoid" Set.AosModok MinionType 4)
  { cdTraits = fromList [T.Adaptoid]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost, B.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50114 :: CardDef
aos50114 = (hoodCardDef "50114" "Automated Mobile Unit" Set.AosModok AttachmentType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50115 :: CardDef
aos50115 = (hoodCardDef "50115" "Focusing Crystal" Set.AosModok AttachmentType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  }

aos50116 :: CardDef
aos50116 = (hoodCardDef "50116" "Nanobots" Set.AosModok AttachmentType 1)
  { cdTraits = fromList [T.Tech]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50117 :: CardDef
aos50117 = (hoodCardDef "50117" "Psionic Force Field" Set.AosModok AttachmentType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50118 :: CardDef
aos50118 = (hoodCardDef "50118" "Psionic Machetes" Set.AosModok AttachmentType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50119 :: CardDef
aos50119 = (hoodCardDef "50119" "Reverse Engineering" Set.AosModok AttachmentType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50120 :: CardDef
aos50120 = (hoodCardDef "50120" "A.I.M. Jailer" Set.AosModok MinionType 2)
  { cdTraits = fromList [T.Aim]
  , cdKeywords = fromList [K.Guard]
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50121 :: CardDef
aos50121 = (hoodCardDef "50121" "Hostage Situation" Set.AosModok SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50122 :: CardDef
aos50122 = (hoodCardDef "50122" "Psionic Enhancement" Set.AosModok SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [K.Hinder (PerPlayer 1)]
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 1
  }

aos50123 :: CardDef
aos50123 = (hoodCardDef "50123" "\"It's Alive!\"" Set.AosModok TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50124 :: CardDef
aos50124 = (hoodCardDef "50124" "Psionic Blast" Set.AosModok TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50125 :: CardDef
aos50125 = (hoodCardDef "50125" "Scientist Supreme" Set.AosScientistSupreme MinionType 1)
  { cdTraits = fromList [T.Aim, T.Genius]
  , cdKeywords = fromList [K.Villainous, K.Vulnerable, K.Victory (-1)]
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50126 :: CardDef
aos50126 = (hoodCardDef "50126" "Monica Rappaccini" Set.AosScientistSupreme MinionType 1)
  { cdTraits = fromList [T.Aim, T.Genius]
  , cdKeywords = fromList [K.Vulnerable, K.Victory (-1)]
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50127 :: CardDef
aos50127 = (hoodCardDef "50127" "Diplomatic Immunity" Set.AosScientistSupreme SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  }

aos50128 :: CardDef
aos50128 = (hoodCardDef "50128" "Diplomatic Sanctions" Set.AosScientistSupreme TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList [K.Surge]
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost, B.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50129a :: CardDef
aos50129a = (hoodCardDef "50129a" "Citizen V" Set.AosThunderbolts VillainType 0)
  { cdTraits = fromList [T.Thunderbolt]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50129b :: CardDef
aos50129b = (hoodCardDef "50129b" "Citizen V" Set.AosThunderbolts VillainType 0)
  { cdTraits = fromList [T.Thunderbolt]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50130b :: CardDef
aos50130b = (hoodCardDef "50130" "Apprehending Rogue Agents" Set.AosThunderbolts MainSchemeType 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50131a :: CardDef
aos50131a = (hoodCardDef "50131a" "Justice, Like Lightning" Set.AosThunderbolts EnvironmentType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50131b :: CardDef
aos50131b = (hoodCardDef "50131b" "Thunderbolt Backup" Set.AosThunderbolts EnvironmentType 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50132 :: CardDef
aos50132 = (hoodCardDef "50132" "Citizen V's Sword" Set.AosThunderbolts AttachmentType 1)
  { cdTraits = fromList [T.Weapon]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50133 :: CardDef
aos50133 = (hoodCardDef "50133" "Jolt" Set.AosThunderbolts MinionType 1)
  { cdTraits = fromList [T.Thunderbolt]
  , cdKeywords = fromList [K.Villainous]
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50134 :: CardDef
aos50134 = (hoodCardDef "50134" "Innocent Bystanders" Set.AosThunderbolts ObligationType 3)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50135 :: CardDef
aos50135 = (hoodCardDef "50135" "The Coming Storm" Set.AosThunderbolts SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 1
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50136 :: CardDef
aos50136 = (hoodCardDef "50136" "Rumbling Thunder" Set.AosThunderbolts SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  }

aos50137 :: CardDef
aos50137 = (hoodCardDef "50137" "Down but Not Out" Set.AosThunderbolts TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50138 :: CardDef
aos50138 = (hoodCardDef "50138" "Tap In" Set.AosThunderbolts TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50139 :: CardDef
aos50139 = (hoodCardDef "50139" "Moonstone" Set.AosGravitationalPull MinionType 1)
  { cdTraits = fromList [T.Aerial, T.Elite, T.Thunderbolt]
  , cdKeywords = fromList [K.Villainous, K.Victory 1]
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50140 :: CardDef
aos50140 = (hoodCardDef "50140" "Rule the Skies" Set.AosGravitationalPull SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost, B.Star]
  , cdHazards = 1
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50141 :: CardDef
aos50141 = (hoodCardDef "50141" "Gravitational Pull" Set.AosGravitationalPull TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50142 :: CardDef
aos50142 = (hoodCardDef "50142" "Psychological Manipulation" Set.AosGravitationalPull TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50143 :: CardDef
aos50143 = (hoodCardDef "50143" "Songbird" Set.AosHardSound MinionType 1)
  { cdTraits = fromList [T.Aerial, T.Elite, T.Thunderbolt]
  , cdKeywords = fromList [K.Villainous, K.Victory 1]
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50144 :: CardDef
aos50144 = (hoodCardDef "50144" "Solid Sound Constructs" Set.AosHardSound AttachmentType 1)
  { cdTraits = fromList [T.Weapon]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 1
  }

aos50145 :: CardDef
aos50145 = (hoodCardDef "50145" "Hard Sound Bindings" Set.AosHardSound AttachmentType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50146 :: CardDef
aos50146 = (hoodCardDef "50146" "Sonic Bubble" Set.AosHardSound SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50147 :: CardDef
aos50147 = (hoodCardDef "50147" "Hard Sound" Set.AosHardSound TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50148 :: CardDef
aos50148 = (hoodCardDef "50148" "Black Widow" Set.AosPaleLittleSpider MinionType 1)
  { cdTraits = fromList [T.Elite, T.Thunderbolt]
  , cdKeywords = fromList [K.Villainous, K.Retaliate 1, K.Victory 1]
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50149 :: CardDef
aos50149 = (hoodCardDef "50149" "Handspring" Set.AosPaleLittleSpider AttachmentType 2)
  { cdTraits = fromList [T.Preparation]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50150 :: CardDef
aos50150 = (hoodCardDef "50150" "Pride of the Red Room" Set.AosPaleLittleSpider SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 1
  }

aos50151 :: CardDef
aos50151 = (hoodCardDef "50151" "Pale Little Spider" Set.AosPaleLittleSpider TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50152 :: CardDef
aos50152 = (hoodCardDef "50152" "Radioactive Man" Set.AosPowerOfTheAtom MinionType 1)
  { cdTraits = fromList [T.Elite, T.Thunderbolt]
  , cdKeywords = fromList [K.Villainous, K.Victory 1]
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50153 :: CardDef
aos50153 = (hoodCardDef "50153" "Radiation Exposure" Set.AosPowerOfTheAtom AttachmentType 2)
  { cdTraits = fromList [T.Condition]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50154 :: CardDef
aos50154 = (hoodCardDef "50154" "Runaway Nuclear Reaction" Set.AosPowerOfTheAtom SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50155 :: CardDef
aos50155 = (hoodCardDef "50155" "Power of the Atom" Set.AosPowerOfTheAtom TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50156 :: CardDef
aos50156 = (hoodCardDef "50156" "MACH-IV" Set.AosSupersonic MinionType 1)
  { cdTraits = fromList [T.Aerial, T.Elite, T.Thunderbolt]
  , cdKeywords = fromList [K.Villainous, K.Victory 1]
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50157 :: CardDef
aos50157 = (hoodCardDef "50157" "Blasters" Set.AosSupersonic AttachmentType 1)
  { cdTraits = fromList [T.Tech, T.Weapon]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50158 :: CardDef
aos50158 = (hoodCardDef "50158" "Heat-Seeking Missiles" Set.AosSupersonic AttachmentType 1)
  { cdTraits = fromList [T.Tech, T.Weapon]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50159 :: CardDef
aos50159 = (hoodCardDef "50159" "Aerial Dogfight" Set.AosSupersonic SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [K.Hinder (PerPlayer 2)]
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 1
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50160 :: CardDef
aos50160 = (hoodCardDef "50160" "Supersonic" Set.AosSupersonic TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50161 :: CardDef
aos50161 = (hoodCardDef "50161" "Batroc" Set.AosTheLeaper MinionType 1)
  { cdTraits = fromList [T.Elite, T.Thunderbolt]
  , cdKeywords = fromList [K.Villainous, K.Victory 1]
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50162 :: CardDef
aos50162 = (hoodCardDef "50162" "Coup de Foudre" Set.AosTheLeaper SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  }

aos50163 :: CardDef
aos50163 = (hoodCardDef "50163" "Batroc the Leaper" Set.AosTheLeaper TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50164 :: CardDef
aos50164 = (hoodCardDef "50164" "Parcours du Combattant" Set.AosTheLeaper TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList [K.Surge]
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50165a :: CardDef
aos50165a = (hoodCardDef "50165a" "Baron Zemo" Set.AosBaronZemo VillainType 0)
  { cdTraits = fromList [T.Thunderbolt]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50165b :: CardDef
aos50165b = (hoodCardDef "50165b" "Baron Zemo" Set.AosBaronZemo VillainType 0)
  { cdTraits = fromList [T.Thunderbolt, T.Unmasked]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50166a :: CardDef
aos50166a = (hoodCardDef "50166a" "Baron Zemo" Set.AosBaronZemo VillainType 0)
  { cdTraits = fromList [T.Thunderbolt]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50166b :: CardDef
aos50166b = (hoodCardDef "50166b" "Baron Zemo" Set.AosBaronZemo VillainType 0)
  { cdTraits = fromList [T.Thunderbolt, T.Unmasked]
  , cdKeywords = fromList [K.Steady]
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50167b :: CardDef
aos50167b = (hoodCardDef "50167" "Zemo's Manipulations" Set.AosBaronZemo MainSchemeType 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50168b :: CardDef
aos50168b = (hoodCardDef "50168" "The Accusation" Set.AosBaronZemo MainSchemeType 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50169b :: CardDef
aos50169b = (hoodCardDef "50169" "Fighting Zemo" Set.AosBaronZemo MainSchemeType 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50170 :: CardDef
aos50170 = (hoodCardDef "50170" "Baron Zemo's Sword" Set.AosBaronZemo AttachmentType 1)
  { cdTraits = fromList [T.Weapon]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50171 :: CardDef
aos50171 = (hoodCardDef "50171" "Reluctant Foe" Set.AosBaronZemo AttachmentType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [K.Villainous]
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50172 :: CardDef
aos50172 = (hoodCardDef "50172" "S.H.I.E.L.D. Agent" Set.AosBaronZemo MinionType 2)
  { cdTraits = fromList [T.Shield]
  , cdKeywords = fromList [K.Quickstrike, K.Vulnerable]
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50173 :: CardDef
aos50173 = (hoodCardDef "50173" "Divided Loyalties" Set.AosBaronZemo SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [K.Hinder (PerPlayer 1)]
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50174 :: CardDef
aos50174 = (hoodCardDef "50174" "Undermine Support" Set.AosBaronZemo SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [K.Hinder (PerPlayer 1)]
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50175 :: CardDef
aos50175 = (hoodCardDef "50175" "Battle of Wits" Set.AosBaronZemo TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50176 :: CardDef
aos50176 = (hoodCardDef "50176" "Might Makes Right" Set.AosBaronZemo TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50177 :: CardDef
aos50177 = (hoodCardDef "50177" "The Ends Justify the Means" Set.AosBaronZemo TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50178 :: CardDef
aos50178 = (hoodCardDef "50178" "S.H.I.E.L.D. Trooper" Set.AosShield MinionType 3)
  { cdTraits = fromList [T.Shield]
  , cdKeywords = fromList [K.Patrol, K.Vulnerable]
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50179 :: CardDef
aos50179 = (hoodCardDef "50179" "Arrest Warrant" Set.AosShield ObligationType 1)
  { cdTraits = fromList [T.Shield]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  }

aos50180 :: CardDef
aos50180 = (hoodCardDef "50180" "Disavowed" Set.AosShield SideSchemeType 1)
  { cdTraits = fromList [T.Shield]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdHazards = 1
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50181a :: CardDef
aos50181a = (hoodCardDef "50181a" "Chief Medical Officer" Set.AosExecutiveBoard EnvironmentType 1)
  { cdTraits = fromList [T.BoardMember]
  , cdKeywords = fromList [K.Setup]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50181b :: CardDef
aos50181b = (hoodCardDef "50181b" "Medical Officer's Aid" Set.AosExecutiveBoard AttachmentType 0)
  { cdTraits = fromList [T.BoardMember]
  , cdKeywords = fromList [K.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50182a :: CardDef
aos50182a = (hoodCardDef "50182a" "Chief Surveillance Officer" Set.AosExecutiveBoard EnvironmentType 1)
  { cdTraits = fromList [T.BoardMember]
  , cdKeywords = fromList [K.Setup]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50182b :: CardDef
aos50182b = (hoodCardDef "50182b" "Surveillance Officer's Aid" Set.AosExecutiveBoard AttachmentType 0)
  { cdTraits = fromList [T.BoardMember]
  , cdKeywords = fromList [K.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50183a :: CardDef
aos50183a = (hoodCardDef "50183a" "Chief Tactical Officer" Set.AosExecutiveBoard EnvironmentType 1)
  { cdTraits = fromList [T.BoardMember]
  , cdKeywords = fromList [K.Setup]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50183b :: CardDef
aos50183b = (hoodCardDef "50183b" "Tactical Officer's Aid" Set.AosExecutiveBoard AttachmentType 0)
  { cdTraits = fromList [T.BoardMember]
  , cdKeywords = fromList [K.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50184a :: CardDef
aos50184a = (hoodCardDef "50184a" "A.I.M. Interference ([energy])" Set.AosExecutiveBoard TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [K.Incite 1]
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50184b :: CardDef
aos50184b = (hoodCardDef "50184b" "A.I.M. Interference ([mental])" Set.AosExecutiveBoard TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [K.Incite 1]
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

aos50184c :: CardDef
aos50184c = (hoodCardDef "50184c" "A.I.M. Interference ([physical])" Set.AosExecutiveBoard TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [K.Incite 1]
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

-- The collection hero's picture is retained while this definition identifies its runner.
aosReluctantFoe :: CardDef
aosReluctantFoe = (hoodCardDef "aosReluctantFoe" "Reluctant Foe" Set.AosBaronZemo MinionType 0)
  {cdTraits=fromList [T.Elite],cdKeywords=fromList [K.Villainous]}

aosControlled :: CardDef
aosControlled = (hoodCardDef "aosControlled" "Controlled" Set.MariaHillNemesis MinionType 0)
  {cdTraits=fromList [T.Controlled]}
