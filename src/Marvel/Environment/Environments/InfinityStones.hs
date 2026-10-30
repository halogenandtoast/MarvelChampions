-- | The Infinity Stone environments (The Mad Titan's Shadow, Infinity Gauntlet set): put
-- into play from the Infinity Stone deck, each with a "Special" that the Infinity
-- Gauntlet resolves after the attached villain activates against a player
-- ('resolveInfinityStone'), the stone then going to the stone deck's discard pile.
module Marvel.Environment.Environments.InfinityStones (
  mindStone,
  MindStone (..),
  infinityPowerStone,
  InfinityPowerStone (..),
  realityStone,
  RealityStone (..),
  soulStone,
  SoulStone (..),
  spaceStone,
  SpaceStone (..),
  timeStone,
  TimeStone (..),
  resolveInfinityStone,
  infinityStonesInPlay,
) where

import Marvel.Prelude

import Data.List (elemIndex, nub, sortOn)
import Marvel.Ability.Types
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait

-- | "Resolve the Special ability" of the stone, for the player the villain activated
-- against.
resolveInfinityStone :: (ReverseQueue m) => IdentityId -> EnvironmentId -> m ()
resolveInfinityStone you eid = cardSpecific (EnvironmentRef eid) "infinityStoneSpecial" you

-- | The Infinity Stones in play, in the order they entered play: an area lists its
-- environments as they enter, each leaving the list as it leaves play.
infinityStonesInPlay :: (HasGame m) => m [EnvironmentId]
infinityStonesInPlay = do
  stones <- selectList (EnvironmentWithTrait InfinityStone)
  entered <- concatMap areaEnvironments <$> getAreas
  pure $ sortOn (`elemIndex` entered) stones

-- | What every stone does but its Special: resolved for the player, then "place this
-- card in the Infinity Stone deck discard pile" (its deck's, by its home).
runInfinityStone :: (ReverseQueue m) => (IdentityId -> m ()) -> Message -> Attrs Environment -> m (Attrs Environment)
runInfinityStone special msg attrs = case msg of
  CardSpecific (isTarget attrs -> True) "infinityStoneSpecial" (fromJSON -> Success you) -> do
    specialAbility you attrs do
      special you
      removeFromPlay attrs
    pure attrs
  _ -> runMessage msg attrs

mindStone :: EnvironmentCard MindStone
mindStone = environment MindStone Cards.mindStone

-- | Special: you are confused. If you were already confused, discard 1 card at random
-- from your hand.
newtype MindStone = MindStone (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage MindStone where
  runMessage msg (MindStone attrs) = runQueueT $ MindStone <$> runInfinityStone special msg attrs
   where
    special you = do
      confused <- identityMatches ConfusedIdentity you
      if confused then discardFrom you RandomFromHand 1 else confuseIdentity you

infinityPowerStone :: EnvironmentCard InfinityPowerStone
infinityPowerStone = environment InfinityPowerStone Cards.infinityPowerStone

-- | Special: you are stunned. If you were already stunned, take 3 damage.
newtype InfinityPowerStone = InfinityPowerStone (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage InfinityPowerStone where
  runMessage msg (InfinityPowerStone attrs) = runQueueT $ InfinityPowerStone <$> runInfinityStone special msg attrs
   where
    special you = do
      stunned <- identityMatches StunnedIdentity you
      if stunned then damageIdentity you attrs (toDamage 3 FromAbility) else stunIdentity you

realityStone :: EnvironmentCard RealityStone
realityStone = environment RealityStone Cards.realityStone

-- | Special: discard an ally, upgrade, or support you control.
newtype RealityStone = RealityStone (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage RealityStone where
  runMessage msg (RealityStone attrs) = runQueueT $ RealityStone <$> runInfinityStone special msg attrs
   where
    special you = do
      allies <- selectList (AllyControlledBy $ IdentityWithId you)
      upgrades <- selectList (UpgradeControlledBy $ IdentityWithId you)
      supports <- selectList (SupportControlledBy $ IdentityWithId you)
      chooseAndDiscardControlled you (map AllyRef allies <> map UpgradeRef upgrades <> map SupportRef supports)

soulStone :: EnvironmentCard SoulStone
soulStone = environment SoulStone Cards.soulStone

-- | Special: heal 3 damage from the villain and give it a facedown boost card.
newtype SoulStone = SoulStone (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage SoulStone where
  runMessage msg (SoulStone attrs) = runQueueT $ SoulStone <$> runInfinityStone special msg attrs
   where
    special _ = selectOne ActiveVillain >>= traverse_ \v -> do
      heal v 3
      dealBoost v

spaceStone :: EnvironmentCard SpaceStone
spaceStone = environment SpaceStone Cards.spaceStone

-- | Special: discard cards from the top of the encounter deck until a minion is
-- discarded, and put that minion into play engaged with you (the scenario does that for
-- a player told the match).
newtype SpaceStone = SpaceStone (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage SpaceStone where
  runMessage msg (SpaceStone attrs) = runQueueT $ SpaceStone <$> runInfinityStone special msg attrs
   where
    special you = discardUntil FromEncounterDeck (CardWithType MinionType) (IdentityRef you)

timeStone :: EnvironmentCard TimeStone
timeStone = environment TimeStone Cards.timeStone

-- | Special: discard the top 4 cards of your deck and place 1 threat on the main scheme
-- for each different card type discarded this way. The discarded cards are told to the
-- stone before it leaves play.
newtype TimeStone = TimeStone (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage TimeStone where
  runMessage msg (TimeStone attrs) = runQueueT $ case msg of
    WithDiscarded (isTarget attrs -> True) _ cards -> do
      let types = fromIntegral . length . nub $ map (cdCardType . getCardDef) cards
      when (types > 0) $ placeThreatOnEach attrs types EncounterMainScheme
      pure $ TimeStone attrs
    _ -> TimeStone <$> runInfinityStone special msg attrs
   where
    special you = discardFromDeck you 4 attrs
