-- | Absorbing Man's environments: each has surge, a Forced Response to his undefended
-- attacks, stronger with 5 or more delay counters on the main scheme, and "Boost: put
-- this card into play". None Shall Pass keeps one in play at a time.
module Marvel.Environment.Environments.AbsorbingMan (
  denseForest,
  DenseForest (..),
  snowyHillside,
  SnowyHillside (..),
  rockyOutcrop,
  RockyOutcrop (..),
  abandonedFacility,
  AbandonedFacility (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Id hiding (EnvironmentId)
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Window

-- | Forced Response: after Absorbing Man makes an undefended attack against you. He is
-- the villain of his scenario.
afterUndefendedAttack :: Attrs Environment -> [Ability]
afterUndefendedAttack a =
  [windowAbility a 1 (EnemyAttacked After (VillainEnemy <> UndefendedEnemy) You) ForcedResponse NoCost $ runAbility a 1]

-- | 1, or 2 with 5 or more delay counters on the main scheme.
delayed :: (HasGame m) => m Natural
delayed = (\n -> if n >= 5 then 2 else 1) <$> getMainSchemeCounters

-- | The forced response's effect for you, and [star] Boost: put this card into play.
runAbsorbing :: (ReverseQueue m) => (IdentityId -> Natural -> m ()) -> Message -> Attrs Environment -> m Bool
runAbsorbing effect msg attrs = case msg of
  RanAbility you (isTarget attrs -> True) 1 _ _ -> True <$ (effect you =<< delayed)
  Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
    putBoostIntoPlay target =<< getBoostedPlayer enemy
    pure True
  _ -> pure False

denseForest :: EnvironmentCard DenseForest
denseForest = environment DenseForest Cards.denseForest

-- | Take 1 indirect damage (2).
newtype DenseForest = DenseForest (Attrs Environment)
  deriving anyclass (IsEnvironment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance HasAbilities DenseForest where
  getAbilities (DenseForest a) = afterUndefendedAttack a

instance RunMessage DenseForest where
  runMessage msg e@(DenseForest attrs) = runQueueT do
    handled <- runAbsorbing (\you n -> dealIndirectDamage you attrs n) msg attrs
    if handled then pure e else DenseForest <$> runMessage msg attrs

snowyHillside :: EnvironmentCard SnowyHillside
snowyHillside = environment SnowyHillside Cards.snowyHillside

-- | Place 1 threat (2) on the main scheme.
newtype SnowyHillside = SnowyHillside (Attrs Environment)
  deriving anyclass (IsEnvironment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance HasAbilities SnowyHillside where
  getAbilities (SnowyHillside a) = afterUndefendedAttack a

instance RunMessage SnowyHillside where
  runMessage msg e@(SnowyHillside attrs) = runQueueT do
    handled <- runAbsorbing (\_ n -> placeThreatOnEach attrs n MainScheme) msg attrs
    if handled then pure e else SnowyHillside <$> runMessage msg attrs

rockyOutcrop :: EnvironmentCard RockyOutcrop
rockyOutcrop = environment RockyOutcrop Cards.rockyOutcrop

-- | Heal 1 damage (2) from him.
newtype RockyOutcrop = RockyOutcrop (Attrs Environment)
  deriving anyclass (IsEnvironment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance HasAbilities RockyOutcrop where
  getAbilities (RockyOutcrop a) = afterUndefendedAttack a

instance RunMessage RockyOutcrop where
  runMessage msg e@(RockyOutcrop attrs) = runQueueT do
    handled <- runAbsorbing (\_ n -> traverse_ (`heal` n) =<< selectOne ActiveVillain) msg attrs
    if handled then pure e else RockyOutcrop <$> runMessage msg attrs

abandonedFacility :: EnvironmentCard AbandonedFacility
abandonedFacility = environment AbandonedFacility Cards.abandonedFacility

-- | Discard 1 resource icon (2) from your hand: a card counts each resource it prints,
-- until that many are discarded or the hand has none.
newtype AbandonedFacility = AbandonedFacility (Attrs Environment)
  deriving anyclass (IsEnvironment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance HasAbilities AbandonedFacility where
  getAbilities (AbandonedFacility a) = afterUndefendedAttack a

instance RunMessage AbandonedFacility where
  runMessage msg e@(AbandonedFacility attrs) = runQueueT case msg of
    CardSpecific (isTarget attrs -> True) "discard" (fromJSON -> Success (you :: IdentityId, n :: Natural)) -> do
      cards <- projectMap PlayerIdentityHand (filter (notNull . printedResources . getCardDef) . unHand) you
      unless (n == 0 || null cards) $
        chooseOnePrompted you "$label.chooseResourceToDiscard"
          [ targeting card do
              discardCard card
              cardSpecific attrs "discard" (you, subtractNatural (fromIntegral $ length $ printedResources $ getCardDef card) n)
          | card <- cards
          ]
      pure e
    _ -> do
      handled <- runAbsorbing (\you n -> cardSpecific attrs "discard" (you, n)) msg attrs
      if handled then pure e else AbandonedFacility <$> runMessage msg attrs
