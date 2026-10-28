{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Minion where

import Marvel.Prelude

import Marvel.Card
import Marvel.Id
import Marvel.Minion.Minions
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types
import Marvel.Minion.Cards qualified as Cards

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
      [ SomeMinionCard hydraMercenary
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
      ]

lookupMinion :: CardCode -> IdentityId -> MinionId -> Minion
lookupMinion cardCode identityId minionId = case lookup cardCode allMinions of
  Just (SomeMinionCard a) -> Minion $ buildCard a (identityId, minionId)
  Nothing -> error $ "Invalid card code for minion " <> show cardCode
