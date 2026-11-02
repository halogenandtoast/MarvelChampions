module Marvel.TricksterTakeover.Cards where

import Marvel.Prelude
import Marvel.Card
import Marvel.Name ((<:>))
import Marvel.Hood.Cards (hoodCardDef)
import Marvel.EncounterSet qualified as Set
import Marvel.Trait qualified as Trait
import Marvel.Resource
import Marvel.Keyword qualified as K
import Marvel.Boost qualified as B

tt55001 :: CardDef
tt55001 = (hoodCardDef "55001" "Enchantress" Set.EnchantressScenario VillainType 0)
  { cdTraits = fromList [Trait.Asgard, Trait.Mystic]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  }

tt55002 :: CardDef
tt55002 = (hoodCardDef "55002" "Enchantress" Set.EnchantressScenario VillainType 0)
  { cdTraits = fromList [Trait.Asgard, Trait.Mystic]
  , cdKeywords = fromList [K.Steady, K.Toughness]
  , cdUnique = True
  , cdBoostIcons = []
  }

tt55003 :: CardDef
tt55003 = (hoodCardDef "55003" "Enchantress" Set.EnchantressScenario VillainType 0)
  { cdTraits = fromList [Trait.Asgard, Trait.Mystic]
  , cdKeywords = fromList [K.Steady, K.Toughness]
  , cdUnique = True
  , cdBoostIcons = []
  }

tt55004a :: CardDef
tt55004a = (hoodCardDef "55004a" "Prime Real Estate" Set.EnchantressScenario MainSchemeType 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55004b"]
  }

tt55004b :: CardDef
tt55004b = (hoodCardDef "55004b" "Prime Real Estate" Set.EnchantressScenario MainSchemeType 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55004a"]
  }

tt55005a :: CardDef
tt55005a = (hoodCardDef "55005a" "Sovereign Sorceress" Set.EnchantressScenario MainSchemeType 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55005b"]
  }

tt55005b :: CardDef
tt55005b = (hoodCardDef "55005b" "Sovereign Sorceress" Set.EnchantressScenario MainSchemeType 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55005a"]
  }

tt55006 :: CardDef
tt55006 = (hoodCardDef "55006" "Future of Despair" Set.EnchantressScenario SideSchemeType 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  }

tt55007a :: CardDef
tt55007a = (hoodCardDef "55007a" "Hypnotic Gaze" Set.EnchantressScenario AttachmentType 0)
  { cdTraits = fromList [Trait.Enchantment]
  , cdKeywords = fromList [K.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55007b"]
  }

tt55007b :: CardDef
tt55007b = (hoodCardDef "55007b" "Trance of Envy" Set.EnchantressScenario AttachmentType 0)
  { cdTraits = fromList [Trait.Enchantment]
  , cdKeywords = fromList [K.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55007a"]
  }

tt55008a :: CardDef
tt55008a = (hoodCardDef "55008a" "Hypnotic Gaze" Set.EnchantressScenario AttachmentType 0)
  { cdTraits = fromList [Trait.Enchantment]
  , cdKeywords = fromList [K.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55008b"]
  }

tt55008b :: CardDef
tt55008b = (hoodCardDef "55008b" "Trance of Greed" Set.EnchantressScenario AttachmentType 0)
  { cdTraits = fromList [Trait.Enchantment]
  , cdKeywords = fromList [K.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55008a"]
  }

tt55009a :: CardDef
tt55009a = (hoodCardDef "55009a" "Hypnotic Gaze" Set.EnchantressScenario AttachmentType 0)
  { cdTraits = fromList [Trait.Enchantment]
  , cdKeywords = fromList [K.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55009b"]
  }

tt55009b :: CardDef
tt55009b = (hoodCardDef "55009b" "Trance of Pride" Set.EnchantressScenario AttachmentType 0)
  { cdTraits = fromList [Trait.Enchantment]
  , cdKeywords = fromList [K.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55009a"]
  }

tt55010a :: CardDef
tt55010a = (hoodCardDef "55010a" "Hypnotic Gaze" Set.EnchantressScenario AttachmentType 0)
  { cdTraits = fromList [Trait.Enchantment]
  , cdKeywords = fromList [K.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55010b"]
  }

tt55010b :: CardDef
tt55010b = (hoodCardDef "55010b" "Trance of Sloth" Set.EnchantressScenario AttachmentType 0)
  { cdTraits = fromList [Trait.Enchantment]
  , cdKeywords = fromList [K.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55010a"]
  }

tt55011a :: CardDef
tt55011a = (hoodCardDef "55011a" "Hypnotic Gaze" Set.EnchantressScenario AttachmentType 0)
  { cdTraits = fromList [Trait.Enchantment]
  , cdKeywords = fromList [K.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55011b"]
  }

tt55011b :: CardDef
tt55011b = (hoodCardDef "55011b" "Trance of Wrath" Set.EnchantressScenario AttachmentType 0)
  { cdTraits = fromList [Trait.Enchantment]
  , cdKeywords = fromList [K.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55011a"]
  }

tt55012 :: CardDef
tt55012 = (hoodCardDef "55012" "Alluring Call" Set.EnchantressScenario AttachmentType 1)
  { cdTraits = fromList [Trait.Temptation]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Star]
  }

tt55013 :: CardDef
tt55013 = (hoodCardDef "55013" "Kiss of Temptation" Set.EnchantressScenario AttachmentType 1)
  { cdTraits = fromList [Trait.Temptation]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Star]
  }

tt55014 :: CardDef
tt55014 = (hoodCardDef "55014" "Love Concoction" Set.EnchantressScenario AttachmentType 1)
  { cdTraits = fromList [Trait.Item, Trait.Temptation]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Star]
  }

tt55015 :: CardDef
tt55015 = (hoodCardDef "55015" "Seduced" Set.EnchantressScenario AttachmentType 1)
  { cdTraits = fromList [Trait.Condition]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  }

tt55016 :: CardDef
tt55016 = (hoodCardDef "55016" "Crown of the Enchantress" Set.EnchantressScenario AttachmentType 1)
  { cdTraits = fromList [Trait.Item]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  }

tt55017 :: CardDef
tt55017 = (hoodCardDef "55017" "Enthralled Lackey" Set.EnchantressScenario MinionType 2)
  { cdTraits = fromList [Trait.Enthralled]
  , cdKeywords = fromList [K.Quickstrike]
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Star]
  }

tt55018 :: CardDef
tt55018 = (hoodCardDef "55018" "Enthralled Brute" Set.EnchantressScenario MinionType 2)
  { cdTraits = fromList [Trait.Brute, Trait.Enthralled]
  , cdKeywords = fromList [K.Guard]
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Star]
  }

tt55019 :: CardDef
tt55019 = (hoodCardDef "55019" "Sindr" Set.EnchantressScenario MinionType 1)
  { cdTraits = fromList [Trait.Elite, Trait.Giant]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  }

tt55020 :: CardDef
tt55020 = (hoodCardDef "55020" "Ulik" Set.EnchantressScenario MinionType 1)
  { cdTraits = fromList [Trait.Brute, Trait.Troll]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost]
  }

tt55021 :: CardDef
tt55021 = (hoodCardDef "55021" "Law of Attraction" Set.EnchantressScenario SideSchemeType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Star]
  , cdAcceleration = 1
  }

tt55022 :: CardDef
tt55022 = (hoodCardDef "55022" "Spellbound" Set.EnchantressScenario SideSchemeType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  }

tt55023 :: CardDef
tt55023 = (hoodCardDef "55023" "\"Do My Bidding\"" Set.EnchantressScenario TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  }

tt55024 :: CardDef
tt55024 = (hoodCardDef "55024" "Magical Restraints" Set.EnchantressScenario TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  }

tt55025 :: CardDef
tt55025 = (hoodCardDef "55025" "Spell Blast" Set.EnchantressScenario TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  }

tt55026 :: CardDef
tt55026 = (hoodCardDef "55026" "Spell Shards" Set.EnchantressScenario TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Star]
  }

tt55027a :: CardDef
tt55027a = (hoodCardDef "55027a" "Loki, God of Lies" Set.GodOfLies VillainType 0)
  { cdTraits = fromList [Trait.Asgard, Trait.Mystic]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55027b"]
  }

tt55027b :: CardDef
tt55027b = (hoodCardDef "55027b" "Loki, God of Lies" Set.GodOfLies VillainType 0)
  { cdTraits = fromList [Trait.Asgard, Trait.Mystic]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55027a"]
  }

tt55028a :: CardDef
tt55028a = (hoodCardDef "55028a" "Worlds Collide" Set.GodOfLies MainSchemeType 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55028b"]
  }

tt55028b :: CardDef
tt55028b = (hoodCardDef "55028b" "Worlds Collide" Set.GodOfLies MainSchemeType 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55028a"]
  }

tt55029a :: CardDef
tt55029a = (hoodCardDef "55029a" "Loki the Rascal" Set.GodOfLies VillainType 0)
  { cdTraits = fromList [Trait.AvatarofLoki]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55029b"]
  }

tt55029b :: CardDef
tt55029b = (hoodCardDef "55029b" "Fading Figment" Set.GodOfLies VillainType 0)
  { cdTraits = fromList [Trait.Illusion]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55029a"]
  }

tt55030a :: CardDef
tt55030a = (hoodCardDef "55030a" "Loki the Miscreant" Set.GodOfLies VillainType 0)
  { cdTraits = fromList [Trait.AvatarofLoki]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55030b"]
  }

tt55030b :: CardDef
tt55030b = (hoodCardDef "55030b" "Fading Figment" Set.GodOfLies VillainType 0)
  { cdTraits = fromList [Trait.Illusion]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55030a"]
  }

tt55031a :: CardDef
tt55031a = (hoodCardDef "55031a" "Loki the Knave" Set.GodOfLies VillainType 0)
  { cdTraits = fromList [Trait.AvatarofLoki]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55031b"]
  }

tt55031b :: CardDef
tt55031b = (hoodCardDef "55031b" "Fading Figment" Set.GodOfLies VillainType 0)
  { cdTraits = fromList [Trait.Illusion]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55031a"]
  }

tt55032a :: CardDef
tt55032a = (hoodCardDef "55032a" "Loki the Wretch" Set.GodOfLies VillainType 0)
  { cdTraits = fromList [Trait.AvatarofLoki]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55032b"]
  }

tt55032b :: CardDef
tt55032b = (hoodCardDef "55032b" "Fading Figment" Set.GodOfLies VillainType 0)
  { cdTraits = fromList [Trait.Illusion]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55032a"]
  }

tt55033a :: CardDef
tt55033a = (hoodCardDef "55033a" "Mischief and Mayhem" Set.GodOfLies MainSchemeType 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55033b"]
  }

tt55033b :: CardDef
tt55033b = (hoodCardDef "55033b" "Mischief and Mayhem" Set.GodOfLies MainSchemeType 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55033a"]
  }

tt55034a :: CardDef
tt55034a = (hoodCardDef "55034a" "Intense Focus" Set.GodOfLies AttachmentType 0)
  { cdTraits = fromList [Trait.Condition]
  , cdKeywords = fromList [K.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55034b"]
  }

tt55034b :: CardDef
tt55034b = (hoodCardDef "55034b" "Total Focus" Set.GodOfLies AttachmentType 0)
  { cdTraits = fromList [Trait.Condition]
  , cdKeywords = fromList [K.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdAlternateCardCodes = ["55034a"]
  }

tt55035 :: CardDef
tt55035 = (hoodCardDef "55035" "Wrapped in Chains" Set.GodOfLies AttachmentType 1)
  { cdTraits = fromList [Trait.Condition]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  }

tt55036 :: CardDef
tt55036 = (hoodCardDef "55036" "Dark Scepter" Set.GodOfLies AttachmentType 1)
  { cdTraits = fromList [Trait.Item, Trait.Weapon]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  }

tt55037 :: CardDef
tt55037 = (hoodCardDef "55037" "Draugr Buddy" Set.GodOfLies MinionType 2)
  { cdTraits = fromList [Trait.Undead]
  , cdKeywords = fromList [K.Guard]
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  }

tt55038 :: CardDef
tt55038 = (hoodCardDef "55038" "Grendell" Set.GodOfLies MinionType 1)
  { cdTraits = fromList [Trait.Brute, Trait.DarkElf]
  , cdKeywords = fromList [K.Guard]
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost]
  }

tt55039 :: CardDef
tt55039 = (hoodCardDef "55039" "Malekith" Set.GodOfLies MinionType 1)
  { cdTraits = fromList [Trait.DarkElf]
  , cdKeywords = fromList [K.Retaliate 1]
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  }

tt55040 :: CardDef
tt55040 = (hoodCardDef "55040" "Minotaur" Set.GodOfLies MinionType 1)
  { cdTraits = fromList [Trait.Brute, Trait.Creature]
  , cdKeywords = fromList [K.Toughness]
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost]
  }

tt55041 :: CardDef
tt55041 = (hoodCardDef "55041" "The Mangog" Set.GodOfLies MinionType 1)
  { cdTraits = fromList [Trait.Brute, Trait.Creature, Trait.Elite]
  , cdKeywords = fromList [K.Victory 1]
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  }

tt55042 :: CardDef
tt55042 = (hoodCardDef "55042" "Fenris Wolf" Set.GodOfLies MinionType 1)
  { cdTraits = fromList [Trait.Creature, Trait.Elite]
  , cdKeywords = fromList [K.Guard, K.Patrol]
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  }

tt55043 :: CardDef
tt55043 = (hoodCardDef "55043" "Hraesvelgr" Set.GodOfLies MinionType 1)
  { cdTraits = fromList [Trait.Creature, Trait.Elite]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  }

tt55044 :: CardDef
tt55044 = (hoodCardDef "55044" "Laufey" Set.GodOfLies MinionType 1)
  { cdTraits = fromList [Trait.Elite, Trait.Giant]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  }

tt55045 :: CardDef
tt55045 = (hoodCardDef "55045" "Aura of Stasis" Set.GodOfLies SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  }

tt55046 :: CardDef
tt55046 = (hoodCardDef "55046" "Door Between Worlds" Set.GodOfLies SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [K.Victory 1]
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdHazards = 1
  , cdAcceleration = 1
  }

tt55047 :: CardDef
tt55047 = (hoodCardDef "55047" "Lofty Goals" Set.GodOfLies SideSchemeType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Star]
  , cdAcceleration = 1
  }

tt55048 :: CardDef
tt55048 = (hoodCardDef "55048" "New Jotunheim" Set.GodOfLies SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  , cdAcceleration = 1
  }

tt55049 :: CardDef
tt55049 = (hoodCardDef "55049" "Dark Arts" Set.GodOfLies TreacheryType 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Star]
  }

tt55050 :: CardDef
tt55050 = (hoodCardDef "55050" "Dirty Trick" Set.GodOfLies TreacheryType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Star]
  }

tt55051 :: CardDef
tt55051 = (hoodCardDef "55051" "Stories and Lies" Set.GodOfLies TreacheryType 3)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Boost]
  }

tt55052 :: CardDef
tt55052 = (hoodCardDef "55052" "Domineering Force" Set.GodOfLies EnvironmentType 0)
  { cdTraits = fromList [Trait.Synergy]
  , cdKeywords = fromList [K.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  }

tt55053 :: CardDef
tt55053 = (hoodCardDef "55053" "Feigned Retreat" Set.GodOfLies EnvironmentType 0)
  { cdTraits = fromList [Trait.Synergy]
  , cdKeywords = fromList [K.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  }

tt55054 :: CardDef
tt55054 = (hoodCardDef "55054" "Mounting Resistance" Set.GodOfLies EnvironmentType 0)
  { cdTraits = fromList [Trait.Synergy]
  , cdKeywords = fromList [K.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  }

tt55055 :: CardDef
tt55055 = (hoodCardDef "55055" "Unified Front" Set.GodOfLies EnvironmentType 0)
  { cdTraits = fromList [Trait.Synergy]
  , cdKeywords = fromList [K.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  }

tt55056 :: CardDef
tt55056 = (hoodCardDef "55056" "Absorbing Man" Set.TricksterMagic MinionType 1)
  { cdTraits = fromList [Trait.Brute, Trait.Elite, Trait.Enthralled]
  , cdKeywords = fromList [K.Guard, K.Victory 1]
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost]
  }

tt55057 :: CardDef
tt55057 = (hoodCardDef "55057" "Titania" Set.TricksterMagic MinionType 1)
  { cdTraits = fromList [Trait.Brute, Trait.Elite, Trait.Enthralled]
  , cdKeywords = fromList [K.Steady, K.Victory 1]
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost, B.Boost]
  }

tt55058 :: CardDef
tt55058 = (hoodCardDef "55058" "Whirlwind" Set.TricksterMagic MinionType 1)
  { cdTraits = fromList [Trait.Elite, Trait.Enthralled]
  , cdKeywords = fromList [K.Quickstrike, K.Victory 1]
  , cdUnique = True
  , cdBoostIcons = [B.Boost]
  }

tt55059 :: CardDef
tt55059 = (hoodCardDef "55059" "Zzzax" Set.TricksterMagic MinionType 1)
  { cdTraits = fromList [Trait.Elite, Trait.Enthralled]
  , cdKeywords = fromList [K.Victory 1, K.Retaliate 1]
  , cdUnique = True
  , cdBoostIcons = [B.Boost, B.Boost]
  }

tt55060 :: CardDef
tt55060 = (hoodCardDef "55060" "The Trickster Tango" Set.TricksterMagic SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost, B.Star]
  , cdAcceleration = 2
  }

tt55061 :: CardDef
tt55061 = (hoodCardDef "55061" "Puppet Master" Set.TricksterMagic SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  , cdHazards = 1
  }

tt55062 :: CardDef
tt55062 = (hoodCardDef "55062" "Love Triangle" Set.TricksterMagic AttachmentType 1)
  { cdTraits = fromList [Trait.Condition]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [B.Boost]
  }

tt55063 :: CardDef
tt55063 = (hoodCardDef "55063" ("Absorbing Man" <:> "Carl Creel") Set.TricksterMagic AllyType 0)
  { cdTraits = fromList [Trait.Defiant]
  , cdKeywords = fromList [K.Victory 0, K.Linked "55056"]
  , cdUnique = True
  , cdBoostIcons = []
  , cdAspect = Nothing
  , cdCost = Just 3
  , cdResources = [(PrintedResource, Wild)]
  }

tt55064 :: CardDef
tt55064 = (hoodCardDef "55064" ("Titania" <:> "Mary MacPherran") Set.TricksterMagic AllyType 0)
  { cdTraits = fromList [Trait.Defiant]
  , cdKeywords = fromList [K.Victory 0, K.Linked "55057"]
  , cdUnique = True
  , cdBoostIcons = []
  , cdAspect = Nothing
  , cdCost = Just 4
  , cdResources = [(PrintedResource, Wild)]
  }

tt55065 :: CardDef
tt55065 = (hoodCardDef "55065" ("Whirlwind" <:> "David Cannon") Set.TricksterMagic AllyType 0)
  { cdTraits = fromList [Trait.Defiant]
  , cdKeywords = fromList [K.Victory 0, K.Linked "55058"]
  , cdUnique = True
  , cdBoostIcons = []
  , cdAspect = Nothing
  , cdCost = Just 4
  , cdResources = [(PrintedResource, Wild)]
  }

tt55066 :: CardDef
tt55066 = (hoodCardDef "55066" "Zzzax" Set.TricksterMagic AllyType 0)
  { cdTraits = fromList [Trait.Defiant]
  , cdKeywords = fromList [K.Victory 0, K.Linked "55059"]
  , cdUnique = True
  , cdBoostIcons = []
  , cdAspect = Nothing
  , cdCost = Just 3
  , cdResources = [(PrintedResource, Wild)]
  }
