module Marvel.EncounterSet where

import Marvel.Prelude

import Marvel.Card.Code

data EncounterSet
  = Rhino
  | BombScare
  | Klaw
  | MastersOfEvil
  | Ultron
  | UnderAttack
  | LegionsOfHydra
  | TheDoomsdayChair
  | Standard
  | ExpertSet
  | SpiderManNemesis
  | CaptainMarvelNemesis
  | SheHulkNemesis
  | IronManNemesis
  | BlackPantherNemesis
  | CaptainAmericaNemesis
  | MsMarvelNemesis
  | BlackWidowNemesis
  | ThorNemesis
  | DoctorStrangeNemesis
  | HulkNemesis
  -- The Green Goblin
  | RiskyBusiness
  | MutagenFormula
  | GoblinGimmicks
  | AMessOfThings
  | PowerDrain
  | RunningInterference
  -- The Wrecking Crew
  | Breakout
  | Wrecker
  | Thunderball
  | Piledriver
  | Bulldozer
  -- The Rise of Red Skull
  | Crossbones
  | ExperimentalWeapons
  | AbsorbingMan
  | Taskmaster
  | Zola
  | RedSkull
  | HydraAssault
  | WeaponMaster
  | HydraPatrol
  | HydraCamp
  | ExpertCampaign
  | HawkeyeNemesis
  | SpiderWomanNemesis
  -- Ant-Man
  | AntManNemesis
  -- Wasp
  | WaspNemesis
  -- Quicksilver
  | QuicksilverNemesis
  -- Scarlet Witch
  | ScarletWitchNemesis
  -- Star-Lord
  | StarLordNemesis
  -- Gamora
  | GamoraNemesis
  -- Groot
  | GrootNemesis
  -- Rocket Raccoon
  | RocketRaccoonNemesis
  -- Drax
  | DraxNemesis
  -- Venom
  | VenomNemesis
  -- The Once and Future Kang
  | Kang
  | ExpertKang
  | Temporal
  | Anachronauts
  | MasterOfTime
  -- The Galaxy's Most Wanted
  | BrotherhoodOfBadoon
  | InfiltrateTheMuseum
  | EscapeTheMuseum
  | Nebula
  | Ronan
  | BadoonHeadhunter
  | BandOfBadoon
  | GalacticArtifacts
  | KreeMilitants
  | MenagerieMedley
  | PowerStone
  | SpacePirates
  | ShipCommand
  -- The Mad Titan's Shadow
  | EbonyMaw
  | TowerDefense
  | Thanos
  | Hela
  | Loki
  | BlackOrderSet
  | ArmiesOfTitan
  | ChildrenOfThanos
  | InfinityGauntlet
  | LegionsOfHel
  | FrostGiants
  | Enchantress
  | MadTitansShadowCampaign
  | SpectrumNemesis
  | AdamWarlockNemesis
  | -- | the campaign's Challenge side schemes
    Challenge
  | -- | the campaign's player cards bought with units
    TheMarket
  | NebulaNemesis
  | WarMachineNemesis
  | ValkyrieNemesis
  | TheHood
  | VisionNemesis
  | GhostSpiderNemesis
  | SpiderManMoralesNemesis
  -- Sinister Motives
  | Sandman
  | Venom
  | Mysterio
  | SinisterSix
  | VenomGoblin
  | CityInChaos
  | DownToEarth
  | GoblinGear
  | GuerrillaTactics
  | OsbornTech
  | PersonalNightmare
  | SinisterAssault
  | SymbioticStrength
  | WhispersOfParanoia
  | BadPublicity
  | CommunityService
  | SnitchesGetStitches
  | ShieldTech
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

-- | The modular encounter sets, any of which a scenario may be played
-- with in place of the one it recommends.
modularSets :: [EncounterSet]
modularSets =
  [ BombScare
  , MastersOfEvil
  , UnderAttack
  , LegionsOfHydra
  , TheDoomsdayChair
  -- The Green Goblin
  , GoblinGimmicks
  , AMessOfThings
  , PowerDrain
  , RunningInterference

  -- The Wrecking Crew

  -- The Rise of Red Skull
  , HydraAssault
  , WeaponMaster
  , HydraPatrol

  -- The Once and Future Kang
  , Temporal
  , Anachronauts
  , MasterOfTime

  -- The Galaxy's Most Wanted
  , BadoonHeadhunter
  , PowerStone
  , ShipCommand
  -- Brotherhood of Badoon
  , BandOfBadoon
  -- Ronan the Accuser
  , KreeMilitants
  -- The Galaxy's Most Wanted: Space Pirates
  , SpacePirates
  -- Infiltrate the Museum
  , GalacticArtifacts
  , MenagerieMedley
  -- The Mad Titan's Shadow
  , BlackOrderSet
  , ArmiesOfTitan
  , ChildrenOfThanos
  , InfinityGauntlet
  , LegionsOfHel
  , FrostGiants
  , Enchantress
  -- Sinister Motives
  , CityInChaos
  , DownToEarth
  , GoblinGear
  , GuerrillaTactics
  , OsbornTech
  , PersonalNightmare
  , SinisterAssault
  , SymbioticStrength
  , WhispersOfParanoia
  ]

getNemesisSet :: CardCode -> EncounterSet
getNemesisSet cardCode = case toBaseCardCode cardCode of
  "01001" -> SpiderManNemesis
  "01010" -> CaptainMarvelNemesis
  "01019" -> SheHulkNemesis
  "01029" -> IronManNemesis
  "01040" -> BlackPantherNemesis
  "03001" -> CaptainAmericaNemesis
  "05001" -> MsMarvelNemesis
  "08001" -> BlackWidowNemesis
  "06001" -> ThorNemesis
  "09001" -> DoctorStrangeNemesis
  "10001" -> HulkNemesis
  "04001" -> HawkeyeNemesis
  "04031" -> SpiderWomanNemesis
  "12001" -> AntManNemesis
  "13001" -> WaspNemesis
  "14001" -> QuicksilverNemesis
  "15001" -> ScarletWitchNemesis
  "17001" -> StarLordNemesis
  "18001" -> GamoraNemesis
  "16001" -> GrootNemesis
  "16029" -> RocketRaccoonNemesis
  "19001" -> DraxNemesis
  "20001" -> VenomNemesis
  "21001" -> SpectrumNemesis
  "21031" -> AdamWarlockNemesis
  "22001" -> NebulaNemesis
  "23001" -> WarMachineNemesis
  "25001" -> ValkyrieNemesis
  "26001" -> VisionNemesis
  "27001" -> GhostSpiderNemesis
  "27030" -> SpiderManMoralesNemesis
  cCode ->
    error $
      "No nemesis set for "
        <> show cCode
        <> " are you sure you've added it to EncounterSet.hs"
