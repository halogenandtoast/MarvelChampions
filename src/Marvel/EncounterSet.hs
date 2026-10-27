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
  -- Drax
  | DraxNemesis
  -- The Once and Future Kang
  | Kang
  | ExpertKang
  | Temporal
  | Anachronauts
  | MasterOfTime
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
  "19001" -> DraxNemesis
  cCode ->
    error $
      "No nemesis set for "
        <> show cCode
        <> " are you sure you've added it to EncounterSet.hs"
