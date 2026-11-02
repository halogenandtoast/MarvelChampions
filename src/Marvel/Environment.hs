{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Environment where

import Marvel.Environment.Environments.AosP50032 as X

import Marvel.Environment.Environments.AgentsOfShield qualified as AosEncounter
import Marvel.Environment.Environments.AgeOfApocalypseStandardIII

import Marvel.Environment.Environments.AgeOfApocalypseSavageLand

import Marvel.Environment.Environments.AgeOfApocalypseGenosha

import Marvel.Environment.Environments.AgeOfApocalypseBlueMoon

import Marvel.Environment.Environments.HoodPack qualified as HoodPack

import Marvel.Environment.Environments.NextEvolutionNextEvolutionCampaign

import Marvel.Environment.Environments.NextEvolutionMorlockSiege

import Marvel.Environment.Environments.Mojo
import Marvel.Environment.Environments.MojoGenres
import Marvel.Environment.Environments.MojoOpening
import Marvel.Environment.Environments.MutantGenesisCampaign
import Marvel.Environment.Environments.SurgeInCrime
import Marvel.Environment.Environments.SmCampaign

import Marvel.Environment.Environments.SmVenomGoblin

import Marvel.Environment.Environments.SmVenom

import Marvel.Environment.Environments.SmSandman

import Marvel.Environment.Environments.AirStatic

import Marvel.Prelude
import Marvel.Environment.Environments.MutantGenesis

import Marvel.Environment.Environments.UltronDronesVision

import Marvel.Ability.Types
import Marvel.Card
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Environment.Environments.AbsorbingMan
import Marvel.Environment.Environments.CriminalEnterprise
import Marvel.Environment.Environments.RogueVessel
import Marvel.Environment.Environments.BadoonShip
import Marvel.Environment.Environments.LibraryLabyrinth
import Marvel.Environment.Environments.KreeCommandShip
-- The Galaxy's Most Wanted: Nebula
import Marvel.Environment.Environments.NebulasShip
-- The Mad Titan's Shadow: Ebony Maw
import Marvel.Environment.Environments.EbonyMawSpells
-- The Mad Titan's Shadow: Tower Defense
import Marvel.Environment.Environments.AvengersTower
-- The Mad Titan's Shadow: Infinity Gauntlet
import Marvel.Environment.Environments.InfinityStones
import Marvel.Id
import Marvel.Message
import Marvel.Modifier

instance FromJSON Environment where
  parseJSON = withObject "Environment" $ \o -> do
    cardDef <- o .: "environmentCardDef"
    withEnvironmentCardCode (cdCardCode cardDef) $
      \(_ :: EnvironmentCard a) -> Environment <$> parseJSON @a (Object o)

withEnvironmentCardCode ::
  CardCode -> (forall a. (IsEnvironment a) => EnvironmentCard a -> r) -> r
withEnvironmentCardCode cCode f = case lookup cCode allEnvironments of
  Nothing -> error "invalid environment"
  Just (SomeEnvironmentCard a) -> f a

allEnvironments :: HashMap CardCode SomeEnvironmentCard
allEnvironments =
  fromList $ map (toFst someEnvironmentCardCode)
    [ SomeEnvironmentCard AosEncounter.aos50090a, SomeEnvironmentCard AosEncounter.aos50090b, SomeEnvironmentCard AosEncounter.aos50105a, SomeEnvironmentCard AosEncounter.aos50106a, SomeEnvironmentCard AosEncounter.aos50107a, SomeEnvironmentCard AosEncounter.aos50108a, SomeEnvironmentCard AosEncounter.aos50109, SomeEnvironmentCard AosEncounter.aos50110, SomeEnvironmentCard AosEncounter.aos50111, SomeEnvironmentCard AosEncounter.aos50112, SomeEnvironmentCard AosEncounter.aos50131a, SomeEnvironmentCard AosEncounter.aos50131b, SomeEnvironmentCard AosEncounter.aos50181a, SomeEnvironmentCard AosEncounter.aos50182a, SomeEnvironmentCard AosEncounter.aos50183a
  , SomeEnvironmentCard aosP50032
  , SomeEnvironmentCard pursuedByThePastFront, SomeEnvironmentCard pursuedByThePastBack, SomeEnvironmentCard theSavageLand, SomeEnvironmentCard genosha, SomeEnvironmentCard blueAreaOfTheMoon
  , SomeEnvironmentCard HoodPack.hood24049, SomeEnvironmentCard HoodPack.hood24049b, SomeEnvironmentCard HoodPack.hood24049a, SomeEnvironmentCard HoodPack.hood24060, SomeEnvironmentCard HoodPack.hood24061, SomeEnvironmentCard HoodPack.hood24062, SomeEnvironmentCard HoodPack.hood24063
  , SomeEnvironmentCard ne40081a, SomeEnvironmentCard ne40081b, SomeEnvironmentCard ne40190b, SomeEnvironmentCard ne40191b, SomeEnvironmentCard ne40192b, SomeEnvironmentCard ne40193b, SomeEnvironmentCard ne40194b, SomeEnvironmentCard ne40195b
  , SomeEnvironmentCard mojo39003a, SomeEnvironmentCard mojo39003b, SomeEnvironmentCard mojo39004a, SomeEnvironmentCard mojo39004b, SomeEnvironmentCard mojo39026a, SomeEnvironmentCard mojo39026b, SomeEnvironmentCard mojo39035, SomeEnvironmentCard mojo39041, SomeEnvironmentCard mojo39047, SomeEnvironmentCard mojo39053, SomeEnvironmentCard mojo39060, SomeEnvironmentCard mojo39066
  , SomeEnvironmentCard mgProtectTheSenator, SomeEnvironmentCard mgSaveTheSchool, SomeEnvironmentCard mgRescueCaptives
  , SomeEnvironmentCard smCityStreets, SomeEnvironmentCard smBellTowerA, SomeEnvironmentCard smBellTowerB, SomeEnvironmentCard smSkiesOverNewYorkB, SomeEnvironmentCard smLowerManhattanB, SomeEnvironmentCard smMidtownManhattanB, SomeEnvironmentCard smUpperManhattanB, SomeEnvironmentCard smPublicOutcryA, SomeEnvironmentCard smPublicOutcryB
  , SomeEnvironmentCard ultronDrones
    -- The Green Goblin
    , SomeEnvironmentCard criminalEnterprise
    , SomeEnvironmentCard stateOfMadness

    -- The Wrecking Crew

    -- The Rise of Red Skull
    , SomeEnvironmentCard denseForest
    , SomeEnvironmentCard snowyHillside
    , SomeEnvironmentCard rockyOutcrop
    , SomeEnvironmentCard abandonedFacility

    -- The Once and Future Kang

    -- The Galaxy's Most Wanted
    , SomeEnvironmentCard rogueVessel
    -- Brotherhood of Badoon
    , SomeEnvironmentCard badoonShip

    -- The Galaxy's Most Wanted: Escape the Museum
    , SomeEnvironmentCard libraryLabyrinth
    , SomeEnvironmentCard museumShip
    -- Ronan the Accuser
    , SomeEnvironmentCard kreeCommandShip
    -- The Galaxy's Most Wanted: Nebula
    , SomeEnvironmentCard nebulasShip
    -- The Mad Titan's Shadow: Ebony Maw
    , SomeEnvironmentCard fireball
    , SomeEnvironmentCard manipulation
    , SomeEnvironmentCard pacification
    , SomeEnvironmentCard rubblestorm
    -- The Mad Titan's Shadow: Tower Defense
    , SomeEnvironmentCard avengersTowerStronghold
    , SomeEnvironmentCard avengersTowerDamaged
    -- The Mad Titan's Shadow: Infinity Gauntlet
    , SomeEnvironmentCard mindStone
    , SomeEnvironmentCard infinityPowerStone
    , SomeEnvironmentCard realityStone
    , SomeEnvironmentCard soulStone
    , SomeEnvironmentCard spaceStone
    , SomeEnvironmentCard timeStone
    , SomeEnvironmentCard ultronDronesVision
  , SomeEnvironmentCard airStatic
  , SomeEnvironmentCard surgeInCrime
    ]

lookupEnvironment :: CardCode -> EnvironmentId -> Environment
lookupEnvironment cardCode = case lookup cardCode allEnvironments of
  Just (SomeEnvironmentCard a) -> Environment <$> buildCard a
  Nothing -> error $ "Invalid card code for environment " <> show cardCode

-- | Each facedown Drone minion has a base SCH, ATK and hit points of 1, and goes to its
-- owner's discard pile when it leaves play.
-- ponytail: those rules are built into the facedown Drone minion and the game's
-- removal from play, since this environment is in play for the whole scenario
ultronDrones :: EnvironmentCard UltronDrones
ultronDrones = environment UltronDrones Cards.ultronDrones

newtype UltronDrones = UltronDrones (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage UltronDrones where
  runMessage msg (UltronDrones attrs) = UltronDrones <$> runMessage msg attrs
