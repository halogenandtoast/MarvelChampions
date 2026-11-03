{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Minion where

import Marvel.CivilWar.Namor qualified as CwN

import Marvel.CivilWar.CloakAndDagger qualified as CwCD

import Marvel.CivilWar.HellsKitchen qualified as CwHK

import Marvel.KreeFanatic qualified as KF
import Marvel.CivilWar.Defenders qualified as CwD
import Marvel.CivilWar.Atlanteans qualified as CwA
import Marvel.CivilWar.MightyAvengers qualified as CwMA
import Marvel.CivilWar.MariaHill qualified as CwMH
import Marvel.CivilWar.Paladin qualified as CwP
import Marvel.CivilWar.HeroesForHire qualified as CwHFH
import Marvel.CivilWar.CapeKiller qualified as CwCK
import Marvel.CivilWar.MartialLaw qualified as CwML
import Marvel.CivilWar.DangerousRecruits qualified as CwDR
import Marvel.Minion.Minions.CampaignPlayersNemesis qualified as CN

import Marvel.Minion.Minions.JessicaLuke qualified as JL
import Marvel.Minion.Minions.AgentsOfShield qualified as AosEncounter

import Marvel.Minion.Minions.TricksterTakeover qualified as TT

import Marvel.Minion.Minions.AgeOfApocalypseUnus

import Marvel.Minion.Minions.AgeOfApocalypseSavageLand

import Marvel.Minion.Minions.AgeOfApocalypsePrelates

import Marvel.Minion.Minions.AgeOfApocalypseOverseer

import Marvel.Minion.Minions.AgeOfApocalypseInfinites

import Marvel.Minion.Minions.AgeOfApocalypseHounds

import Marvel.Minion.Minions.AgeOfApocalypseGenosha

import Marvel.Minion.Minions.AgeOfApocalypseDarkRiders

import Marvel.Minion.Minions.AgeOfApocalypseClanAkkaba

import Marvel.Minion.Minions.AgeOfApocalypseBlueMoon

import Marvel.Minion.Minions.AgeOfApocalypseAgeOfApocalypse

import Marvel.Minion.Minions.HoodPack qualified as HoodPack

import Marvel.Minion.Minions.NextEvolutionNeStryfe

import Marvel.Minion.Minions.NextEvolutionNextEvolutionCampaign

import Marvel.Minion.Minions.NextEvolutionNastyBoys

import Marvel.Minion.Minions.NextEvolutionMutantSlayers

import Marvel.Minion.Minions.NextEvolutionMutantInsurrection

import Marvel.Minion.Minions.NextEvolutionMisterSinister

import Marvel.Minion.Minions.NextEvolutionExtremeMeasures

import Marvel.Minion.Minions.NextEvolutionBlackTomCassidy

import Marvel.Minion.Minions.Mojo
import Marvel.Minion.Minions.MojoGenres
import Marvel.Minion.Minions.MojoOpening
import Marvel.Minion.Minions.TheHood qualified as Hood
import Marvel.Prelude

import Marvel.Card.Builder ()

import Marvel.Card
import Marvel.Id
import Marvel.Minion.Minions
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types

instance FromJSON Minion where
  parseJSON = withObject "Minion" $ \o -> do
    -- an ally treated as a minion shows the ally's card: its entity is the original's
    cardDef <- o .: "minionOriginalCardDef"
    withMinionCardCode (cdCardCode cardDef) $
      \(_ :: MinionCard a) -> Minion <$> parseJSON @a (Object o)

withMinionCardCode ::
  CardCode -> (forall a. (IsMinion a) => MinionCard a -> r) -> r
withMinionCardCode cCode f = case lookup cCode allMinions of
  Nothing -> error "invalid minion"
  Just (SomeMinionCard a) -> f a

allMinions :: HashMap CardCode SomeMinionCard
allMinions =
  fromList $
    map
      (toFst someMinionCardCode)
      $ [SomeMinionCard CwN.namor] <> map SomeMinionCard CwCD.cloakAndDaggerMinions <> [SomeMinionCard CwHK.daredevil,SomeMinionCard CwHK.ironFist] <> map SomeMinionCard CwD.defenders <> map SomeMinionCard CwHFH.hiredHeroes <> map SomeMinionCard CwMA.minions <> [SomeMinionCard CwA.atlanteanGuard,SomeMinionCard CwP.paladin,SomeMinionCard CwCK.capeKiller,SomeMinionCard CwML.shieldSoldier,SomeMinionCard CwMH.mariaHill,SomeMinionCard CwDR.bullseye,SomeMinionCard CwDR.venom] <> [ SomeMinionCard CN.cn56024, SomeMinionCard CN.cn56027, SomeMinionCard CN.cn56028, SomeMinionCard CN.cn56055, SomeMinionCard CN.cn60033, SomeMinionCard CN.cn60061, SomeMinionCard CN.cn60063
  , SomeMinionCard KF.ronanKreeFanatic
  , SomeMinionCard wm58026, SomeMinionCard hc59036, SomeMinionCard hc59037, SomeMinionCard hc59042, SomeMinionCard hc59043, SomeMinionCard hc59044
  , SomeMinionCard JL.jl61031, SomeMinionCard JL.jl62029, SomeMinionCard JL.jl62032, SomeMinionCard JL.jl62033
  , SomeMinionCard TT.tt55017, SomeMinionCard TT.tt55018, SomeMinionCard TT.tt55019, SomeMinionCard TT.tt55020, SomeMinionCard TT.tt55037, SomeMinionCard TT.tt55038, SomeMinionCard TT.tt55039, SomeMinionCard TT.tt55040, SomeMinionCard TT.tt55041, SomeMinionCard TT.tt55042, SomeMinionCard TT.tt55043, SomeMinionCard TT.tt55044, SomeMinionCard TT.tt55056, SomeMinionCard TT.tt55057, SomeMinionCard TT.tt55058, SomeMinionCard TT.tt55059
  , SomeMinionCard AosEncounter.aosControlledMinion
  , SomeMinionCard AosEncounter.aosReluctantFoeMinion
  , SomeMinionCard AosEncounter.aos50072, SomeMinionCard AosEncounter.aos50073, SomeMinionCard AosEncounter.aos50080, SomeMinionCard AosEncounter.aos50083, SomeMinionCard AosEncounter.aos50084, SomeMinionCard AosEncounter.aos50093, SomeMinionCard AosEncounter.aos50094, SomeMinionCard AosEncounter.aos50098, SomeMinionCard AosEncounter.aos50099, SomeMinionCard AosEncounter.aos50100, SomeMinionCard AosEncounter.aos50113, SomeMinionCard AosEncounter.aos50120, SomeMinionCard AosEncounter.aos50125, SomeMinionCard AosEncounter.aos50126, SomeMinionCard AosEncounter.aos50133, SomeMinionCard AosEncounter.aos50139, SomeMinionCard AosEncounter.aos50143, SomeMinionCard AosEncounter.aos50148, SomeMinionCard AosEncounter.aos50152, SomeMinionCard AosEncounter.aos50156, SomeMinionCard AosEncounter.aos50161, SomeMinionCard AosEncounter.aos50172, SomeMinionCard AosEncounter.aos50178
  , SomeMinionCard fw53030, SomeMinionCard fw53032, SomeMinionCard fw53038, SomeMinionCard fw54028, SomeMinionCard fw54031, SomeMinionCard fw54034
  , SomeMinionCard aosP50030, SomeMinionCard aosP50060, SomeMinionCard aosP50062
  , SomeMinionCard klawShuri, SomeMinionCard mUSICShuri, SomeMinionCard joystickShuri, SomeMinionCard morlunSilk, SomeMinionCard atlasSilk
  , SomeMinionCard nc48027, SomeMinionCard nc48034, SomeMinionCard nc48035, SomeMinionCard nc48036, SomeMinionCard nc48037
  , SomeMinionCard jubilee47024, SomeMinionCard jubilee47030
  , SomeMinionCard sauron
  , SomeMinionCard pyroIceman
  , SomeMinionCard exodusMagneto, SomeMinionCard fabianCortez, SomeMinionCard frenzy, SomeMinionCard sebastianShaw, SomeMinionCard selene, SomeMinionCard hellfirePawnMagneto
  , SomeMinionCard infiniteHunter, SomeMinionCard infiniteSoldier, SomeMinionCard ahab, SomeMinionCard hound, SomeMinionCard gauntlet, SomeMinionCard barrage, SomeMinionCard hardDrive, SomeMinionCard tusk, SomeMinionCard psynapse, SomeMinionCard pterosaur, SomeMinionCard velociraptor, SomeMinionCard giantApe, SomeMinionCard magistrate, SomeMinionCard armoredUnibike, SomeMinionCard genoshanMech, SomeMinionCard gladiator, SomeMinionCard oracle, SomeMinionCard manta, SomeMinionCard earthquake, SomeMinionCard warstar, SomeMinionCard ozymandias, SomeMinionCard scarab, SomeMinionCard clanAkkabaZealot, SomeMinionCard agentOfApocalypse, SomeMinionCard misterSinisterMinion, SomeMinionCard misterSinisterOverseer, SomeMinionCard theShadowKingMinion, SomeMinionCard theShadowKingOverseer, SomeMinionCard abyssMinion, SomeMinionCard abyssOverseer, SomeMinionCard sugarManMinion, SomeMinionCard sugarManOverseer, SomeMinionCard mikhailRasputinMinion, SomeMinionCard mikhailRasputinOverseer
  , SomeMinionCard witchfire
  , SomeMinionCard sym
  , SomeMinionCard belasco
  , SomeMinionCard trevorFitzroy, SomeMinionCard bantam
  , SomeMinionCard butler, SomeMinionCard mutatedSoldier, SomeMinionCard dreadpool
  , SomeMinionCard ladyDeathstrikeX23
  , SomeMinionCard harpoon
  , SomeMinionCard chimera
  , SomeMinionCard HoodPack.hood24015, SomeMinionCard HoodPack.hood24016, SomeMinionCard HoodPack.hood24018, SomeMinionCard HoodPack.hood24024, SomeMinionCard HoodPack.hood24025, SomeMinionCard HoodPack.hood24026, SomeMinionCard HoodPack.hood24027, SomeMinionCard HoodPack.hood24034, SomeMinionCard HoodPack.hood24035, SomeMinionCard HoodPack.hood24041, SomeMinionCard HoodPack.hood24043, SomeMinionCard HoodPack.hood24044, SomeMinionCard HoodPack.hood24045, SomeMinionCard HoodPack.hood24046, SomeMinionCard HoodPack.hood24047, SomeMinionCard HoodPack.hood24065, SomeMinionCard HoodPack.hood24066, SomeMinionCard HoodPack.hood24067, SomeMinionCard HoodPack.hood24068
  , SomeMinionCard ne40094, SomeMinionCard ne40095, SomeMinionCard ne40096, SomeMinionCard ne40097, SomeMinionCard ne40098, SomeMinionCard ne40099, SomeMinionCard ne40100, SomeMinionCard ne40112, SomeMinionCard ne40113, SomeMinionCard ne40114, SomeMinionCard ne40115, SomeMinionCard ne40116, SomeMinionCard ne40132, SomeMinionCard ne40133, SomeMinionCard ne40145, SomeMinionCard ne40174, SomeMinionCard ne40180, SomeMinionCard ne40181, SomeMinionCard ne40182, SomeMinionCard ne40183, SomeMinionCard ne40185, SomeMinionCard ne40186, SomeMinionCard ne40187, SomeMinionCard ne40188, SomeMinionCard ne40198, SomeMinionCard ne40199, SomeMinionCard ne40200, SomeMinionCard ne40201
  , SomeMinionCard prototype
  , SomeMinionCard topaz
  , SomeMinionCard cable40032
  , SomeMinionCard mojo39007, SomeMinionCard mojo39029, SomeMinionCard mojo39042, SomeMinionCard mojo39043, SomeMinionCard mojo39044, SomeMinionCard mojo39049, SomeMinionCard mojo39050, SomeMinionCard mojo39051, SomeMinionCard mojo39052, SomeMinionCard mojo39054, SomeMinionCard mojo39055, SomeMinionCard mojo39056, SomeMinionCard mojo39057, SomeMinionCard mojo39058, SomeMinionCard mojo39068, SomeMinionCard mojo39069
  , SomeMinionCard rogueMystique, SomeMinionCard donaldPierce, SomeMinionCard skullbuster, SomeMinionCard bonebreaker, SomeMinionCard wadeCole, SomeMinionCard murrayReese, SomeMinionCard belladonna, SomeMinionCard guildAssassin, SomeMinionCard exodus
  , SomeMinionCard callisto
  , SomeMinionCard theShadowKing
  , SomeMinionCard misterSinister
  , SomeMinionCard omegaRed, SomeMinionCard ladyDeathstrike
  , SomeMinionCard mgAvalanche, SomeMinionCard mgBlob, SomeMinionCard mgPyro, SomeMinionCard mgToad, SomeMinionCard mgMystique, SomeMinionCard mgSentinelMarkIv, SomeMinionCard mgSentinelMarkIi, SomeMinionCard mgSentinelMarkIii, SomeMinionCard mgSentinelMarkV, SomeMinionCard mgSentinelMarkVi, SomeMinionCard mgSentinelMarkViii, SomeMinionCard mgMTypeSentinel, SomeMinionCard mgFabianCortez, SomeMinionCard mgAmeliaVoght, SomeMinionCard mgSenyaka, SomeMinionCard mgDelgado, SomeMinionCard mgUnuscione, SomeMinionCard mgNimrod, SomeMinionCard mgBastion, SomeMinionCard smSandClone, SomeMinionCard smShiftingApparition, SomeMinionCard smSymbioticBerserker, SomeMinionCard smSymbioticMonstrosity, SomeMinionCard smSymbioticThrall, SomeMinionCard smRhino, SomeMinionCard smCommonCriminal, SomeMinionCard smLifeSizeDecoy, SomeMinionCard smEvilDoppelganger, SomeMinionCard smDoctorOctopus, SomeMinionCard smElectro, SomeMinionCard smHobgoblin, SomeMinionCard smKravenTheHunter, SomeMinionCard smScorpion, SomeMinionCard smVulture, SomeMinionCard smEnragedSymbiote
  , SomeMinionCard Hood.madameMasque
      , SomeMinionCard hydraMercenary
      , SomeMinionCard sandman
      , SomeMinionCard shocker
      , SomeMinionCard hydraBomber
      , SomeMinionCard armoredGuard
      , SomeMinionCard weaponsRunner
      , SomeMinionCard radioactiveMan
      , SomeMinionCard whirlwind
      , SomeMinionCard tigerShark
      , SomeMinionCard melter
      , SomeMinionCard drone
      , SomeMinionCard advancedUltronDrone
      , SomeMinionCard killmonger
      , SomeMinionCard titania
      , SomeMinionCard vulture
      , SomeMinionCard whiplash
      , SomeMinionCard yonRogg
      , SomeMinionCard madameHydra
      , SomeMinionCard hydraSoldier
      , SomeMinionCard modok
      , SomeMinionCard baronZemo
      , SomeMinionCard (reprintAs Cards.hydraSoldierCaptainAmerica hydraSoldier)
      , SomeMinionCard thomasEdison
      , SomeMinionCard edisonsGiantRobot
      , SomeMinionCard taskmaster
      , SomeMinionCard (reprintAs Cards.hydraMercenaryBlackWidow hydraMercenary)
      , SomeMinionCard loki
      , SomeMinionCard frostGiant
      , SomeMinionCard baronMordo
      , SomeMinionCard abomination
      -- The Green Goblin
      , SomeMinionCard goblinKnight
      , SomeMinionCard goblinSoldier
      , SomeMinionCard goblinThrall
      , SomeMinionCard hiredGun
      , SomeMinionCard monster
      , SomeMinionCard privateSecuritySpecialist
      , SomeMinionCard scorpion
      , SomeMinionCard electro
      , SomeMinionCard tombstone

      -- The Wrecking Crew
      , SomeMinionCard corruptPrisonGuard
      , SomeMinionCard (reprintAs Cards.corruptPrisonGuardThunderball corruptPrisonGuard)
      , SomeMinionCard (reprintAs Cards.corruptPrisonGuardPiledriver corruptPrisonGuard)
      , SomeMinionCard (reprintAs Cards.corruptPrisonGuardBulldozer corruptPrisonGuard)
      , SomeMinionCard escapedConvict
      , SomeMinionCard (reprintAs Cards.escapedConvictThunderball escapedConvict)
      , SomeMinionCard (reprintAs Cards.escapedConvictPiledriver escapedConvict)
      , SomeMinionCard (reprintAs Cards.escapedConvictBulldozer escapedConvict)

      -- The Rise of Red Skull
      , SomeMinionCard crossfire
      , SomeMinionCard theViper
      , SomeMinionCard hydraRegular
      , SomeMinionCard (reprintAs Cards.hydraBomberCrossbones hydraBomber)
      , SomeMinionCard hydraFlameSoldier
      , SomeMinionCard hydraJetTrooper
      , SomeMinionCard (reprintAs Cards.hydraRegularHydraPatrol hydraRegular)
      , SomeMinionCard (reprintAs Cards.hydraSoldierHydraPatrol hydraSoldier)

      -- Zola
      , SomeMinionCard ultimateBioServant
      , SomeMinionCard zolasMutate
      , SomeMinionCard berserkMutate

      -- Red Skull
      , SomeMinionCard theSleeper
      , SomeMinionCard hydraExoSoldier

      -- Taskmaster
      , SomeMinionCard hydraHunter

      -- The Once and Future Kang
      , SomeMinionCard ancientWarrior
      , SomeMinionCard chitauriSoldier
      , SomeMinionCard tyrannosaurusRex
      , SomeMinionCard apocryphus
      , SomeMinionCard deathunt9000
      , SomeMinionCard sirRaston
      , SomeMinionCard terminatrix
      , SomeMinionCard wildrun
      , SomeMinionCard kangMasterOfTime
      , SomeMinionCard timeDisplacedSoldier
      , SomeMinionCard macrobots


      -- Ant-Man
      , SomeMinionCard yellowjacket

      -- Wasp
      , SomeMinionCard beetle

      -- Quicksilver
      , SomeMinionCard avalanche

      -- Scarlet Witch
      , SomeMinionCard luminous

      -- Star-Lord
      , SomeMinionCard misterKnife

      -- Gamora
      , SomeMinionCard nebulaMinion

      -- Groot
      , SomeMinionCard furnax

      -- Rocket Raccoon
      , SomeMinionCard blackjackOHare

      -- The Galaxy's Most Wanted
      , SomeMinionCard badoonHeadhunter
      , SomeMinionCard headhuntersHenchman
      -- Brotherhood of Badoon
      , SomeMinionCard badoonEngineer
      -- Band of Badoon
      , SomeMinionCard badoonAssassin
      , SomeMinionCard badoonGrunt
      , SomeMinionCard badoonLieutenant
      , SomeMinionCard badoonSentry
      , SomeMinionCard badoonWarlord
      -- Kree Militants
      , SomeMinionCard kreeCommando
      , SomeMinionCard kreeLieutenant
      , SomeMinionCard kreePrivate
      -- The Galaxy's Most Wanted: Space Pirates
      , SomeMinionCard pirateCommander
      , SomeMinionCard pirateLackey

      -- The Galaxy's Most Wanted: Infiltrate the Museum, Menagerie Medley
      , SomeMinionCard monarkStarstalker
      , SomeMinionCard psionicGhost
      , SomeMinionCard servantBot
      , SomeMinionCard starshark
      -- Drax
      , SomeMinionCard yotatTheDestroyer

      -- Venom
      , SomeMinionCard enragedSymbiote

      -- Spectrum
      , SomeMinionCard radioactiveManNemesis

      -- Adam Warlock
      , SomeMinionCard theMagus
      , SomeMinionCard zealotOfTruth

      -- The Mad Titan's Shadow: Black Order, Armies of Titan
      , SomeMinionCard blackDwarf
      , SomeMinionCard supergiant
      , SomeMinionCard blackOrderInfantry
      , SomeMinionCard outrider

      -- The Mad Titan's Shadow: Tower Defense
      , SomeMinionCard blackOrderBesieger

      -- The Mad Titan's Shadow: Children of Thanos
      , SomeMinionCard corvusGlaiveMinion
      , SomeMinionCard proximaMidnightMinion
      , SomeMinionCard ebonyMawMinion

      -- The Mad Titan's Shadow: Hela, Legions of Hel, Frost Giants, Enchantress
      , SomeMinionCard allyMinion
      , SomeMinionCard garm
      , SomeMinionCard skurge
      , SomeMinionCard nidhogg
      , SomeMinionCard draugr
      , SomeMinionCard laufey
      , SomeMinionCard (reprintAs Cards.frostGiantFrostGiants frostGiant)
      , SomeMinionCard enchantress

      -- The Mad Titan's Shadow: the campaign set
      , SomeMinionCard blackSwan
      , SomeMinionCard gamoraNemesis
      , SomeMinionCard livingLaser
      , SomeMinionCard (reprintAs Cards.enchantressValkyrie enchantress)
      , SomeMinionCard ultronNemesis
  , SomeMinionCard theLizard
  , SomeMinionCard prowler
  , SomeMinionCard warbringer
  , SomeMinionCard armadillo
  , SomeMinionCard luciaVonBardas
  , SomeMinionCard zzzax
      , SomeMinionCard theGreenGobbler
  , SomeMinionCard bora
  , SomeMinionCard brix
  , SomeMinionCard daemos
  , SomeMinionCard jennix
  , SomeMinionCard karn
  , SomeMinionCard morlun
  , SomeMinionCard solus
  , SomeMinionCard verna
  , SomeMinionCard morbius
  , SomeMinionCard bombshellSinister
  , SomeMinionCard electroSinister
  , SomeMinionCard hobgoblinSinister
  , SomeMinionCard ironSpider
  , SomeMinionCard sandmanSinister
  , SomeMinionCard spot
  , SomeMinionCard juggernaut
  , SomeMinionCard whiteQueenMutantGenesis
  , SomeMinionCard hellfirePawn
  , SomeMinionCard darkPhoenix
  ]

lookupMinion :: CardCode -> IdentityId -> MinionId -> Minion
lookupMinion cardCode identityId minionId = case lookup cardCode allMinions of
  Just (SomeMinionCard a) -> Minion $ buildCard a (identityId, minionId)
  Nothing -> error $ "Invalid card code for minion " <> show cardCode
