{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Environment where

import Marvel.Prelude

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
    [ SomeEnvironmentCard ultronDrones
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
