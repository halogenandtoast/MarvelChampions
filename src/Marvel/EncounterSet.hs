module Marvel.EncounterSet where

import Marvel.Prelude

import Marvel.Card.Code

data EncounterSet
  = KreeFanatic
  | TigraNemesis
  | HulklingNemesis
  | DaredevilNemesis
  | EchoNemesis
  | WonderManNemesisSet
  | HerculesNemesisSet
  | AllVersusAllSet
  | JessicaJonesNemesis
  | LukeCageNemesis
  | BlackPantherShuriNemesis
  | BlackPantherShuriObligation
  | SilkNemesis
  | SilkObligation
  | ExtremeRiskSet
  | GrowingStrongSet
  | FalconNemesis
  | WinterSoldierNemesis
  | TechnoSet
  | WhiteoutSet
  | MariaHillNemesis
  | MariaHillObligation
  | NickFuryNemesis
  | NickFuryObligation
  | AosBlackWidow
  | AosBatroc
  | AosModok
  | AosThunderbolts
  | AosBaronZemo
  | AosAimAbduction
  | AosAimScience
  | AosBatrocsBrigade
  | AosScientistSupreme
  | AosGravitationalPull
  | AosHardSound
  | AosPaleLittleSpider
  | AosPowerOfTheAtom
  | AosSupersonic
  | AosTheLeaper
  | AosShield
  | AosExecutiveBoard
  | AosEvidence
  | SauronSet
  | Sabretooth | Brotherhood | Mystique | ProjectWideawake | ZeroTolerance | Sentinels | MasterMold | MansionAttack | Magneto | Acolytes | FuturePast
  | AoaCampaign
  | AoaBasicCampaign
  | AoaOverseer
  | AoaMission
  | AgeOfApocalypse
  | StandardIII
  | AoaClanAkkaba
  | AoaCelestialTech
  | AoaEnSabahNur
  | AoaSavageLand
  | AoaGenosha
  | AoaBlueMoon
  | AoaDarkBeast
  | AoaDarkRiders
  | AoaPrelates
  | AoaApocalypse
  | AoaHounds
  | AoaFourHorsemen
  | AoaDystopianNightmare
  | AoaInfinites
  | AoaUnus
  | NextEvolutionCampaign
  | AngelNemesis
  | IcemanNemesis
  | IcemanObligation
  | JubileeNemesis
  | JubileeObligation
  | NightcrawlerNemesis
  | NightcrawlerObligation
  | MagnetoNemesis
  | MagnetoObligation
  | HellfireSet
  | ArcadeSet
  | CrazyGangSet
  | MagikNemesis
  | BishopNemesis
  | BishopObligation
  | MagikObligation
  | PsylockeNemesis
  | CableNemesis
  | DominoNemesis
  | DominoObligation
  | MutantInsurrection
  | ExtremeMeasures
  | NeStryfe
  | TelepathySet
  | SuperStrengthSet
  | FlightSet
  | MisterSinister
  | BlackTomCassidy
  | HopeSummersSet
  | NeJuggernaut
  | NastyBoys
  | OnTheRun
  | MutantSlayers
  | MilitaryGrade
  | MorlockSiege
  | NeMarauders
  | MutantGenesisCampaign
  | RogueNemesisSet
  | ReaversSet
  | CyclopsNemesis
  | ColossusNemesis
  | PhoenixNemesis
  | ShadowcatNemesis
  | Magog | Spiral | Mojo | Crime | Fantasy | Horror | SciFi | Sitcom | Western | LongshotSet
  | Rhino
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
  | BeastyBoys | BrothersGrimm | CrossfiresCrew | MisterHydeSet | RansackedArmory | SinisterSyndicate | StateOfEmergency | StreetsOfMayhem | WreckingCrewModular | StandardII | ExpertII
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
  | IronheartNemesis
  | ZzzaxSet
  | NovaNemesis
  | Armadillo
  | SpiderHamNemesis
  | Inheritors
  | SpdrNemesis
  | IronSpiderSinisterSix
  | WolverineNemesis
  | LadyDeathstrikeSet
  | StormNemesis
  | ShadowKingSet
  | GambitNemesisSet
  | ExodusSet
  | X23Nemesis
  | DeadpoolNemesis
  | DreadpoolSet
  | EnchantressScenario | GodOfLies | TricksterMagic
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

-- | The modular encounter sets, any of which a scenario may be played
-- with in place of the one it recommends.
modularSets :: [EncounterSet]
modularSets =
  [ KreeFanatic
  , AllVersusAllSet
  , ExtremeRiskSet, GrowingStrongSet, TechnoSet, WhiteoutSet
  , AosAimAbduction, AosAimScience, AosBatrocsBrigade, AosScientistSupreme
  , AosGravitationalPull, AosHardSound, AosPaleLittleSpider, AosPowerOfTheAtom, AosSupersonic, AosTheLeaper
  , SauronSet, HellfireSet, ArcadeSet, CrazyGangSet
  , AoaInfinites, AoaDystopianNightmare, AoaHounds, AoaPrelates, AoaDarkRiders, AoaBlueMoon, AoaGenosha, AoaSavageLand, AoaCelestialTech, AoaClanAkkaba
  , MilitaryGrade, MutantSlayers, NastyBoys, BlackTomCassidy, HopeSummersSet, FlightSet, SuperStrengthSet, TelepathySet, ExtremeMeasures, MutantInsurrection
  , Crime, Fantasy, Horror, SciFi, Sitcom, Western, LongshotSet
  , BombScare
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
  , Armadillo
  , ZzzaxSet
  , Inheritors
  , IronSpiderSinisterSix
  , LadyDeathstrikeSet
  , ShadowKingSet
  , ReaversSet
  , ExodusSet
  -- The Hood
  , TricksterMagic
  , BeastyBoys, BrothersGrimm, CrossfiresCrew, MisterHydeSet, RansackedArmory, SinisterSyndicate, StateOfEmergency, StreetsOfMayhem, WreckingCrewModular
  -- Mutant Genesis
  , Brotherhood
  , Mystique
  , ZeroTolerance
  , Sentinels
  , Acolytes
  , FuturePast
  ]

getNemesisSet :: CardCode -> EncounterSet
getNemesisSet cardCode = case toBaseCardCode cardCode of
  "56001" -> TigraNemesis
  "56029" -> HulklingNemesis
  "60001" -> DaredevilNemesis
  "60037" -> EchoNemesis
  "58001" -> WonderManNemesisSet
  "59001" -> HerculesNemesisSet
  "61001" -> JessicaJonesNemesis
  "62001" -> LukeCageNemesis
  "50001" -> MariaHillNemesis
  "50034" -> NickFuryNemesis
  "51001" -> BlackPantherShuriNemesis
  "52001" -> SilkNemesis
  "53001" -> FalconNemesis
  "54001" -> WinterSoldierNemesis
  "46001" -> IcemanNemesis
  "47001" -> JubileeNemesis
  "48001" -> NightcrawlerNemesis
  "49001" -> MagnetoNemesis
  "45030" -> MagikNemesis
  "45001" -> BishopNemesis
  "44001" -> DeadpoolNemesis
  "43001" -> X23Nemesis
  "42001" -> AngelNemesis
  "38001" -> RogueNemesisSet
  "37001" -> GambitNemesisSet
  "33001" -> CyclopsNemesis
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
  "29001" -> IronheartNemesis
  "29002" -> IronheartNemesis
  "29003" -> IronheartNemesis
  "30001" -> SpiderHamNemesis
  "28001" -> NovaNemesis
  "31001" -> SpdrNemesis
  "31002" -> SpdrNemesis
  "32001" -> ColossusNemesis
  "32030" -> ShadowcatNemesis
  "34001" -> PhoenixNemesis
  "36001" -> StormNemesis
  "35001" -> WolverineNemesis
  "40001" -> CableNemesis
  "40037" -> DominoNemesis
  "41001" -> PsylockeNemesis
  cCode ->
    error $
      "No nemesis set for "
        <> show cCode
        <> " are you sure you've added it to EncounterSet.hs"
