module Marvel.SideScheme.SideSchemes.GoblinNation (
  goblinNation,
  GoblinNation (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import Marvel.Trait (Trait (Goblin))

goblinNation :: SideSchemeCard GoblinNation
goblinNation = sideScheme GoblinNation Cards.goblinNation (PerPlayer 2)

newtype GoblinNation = GoblinNation (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Each Goblin enemy (Green Goblin included) gets +1 ATK.
instance HasModifiersFor GoblinNation where
  getModifiersFor _ target _ = case target of
    VillainRef vid -> goblin (EnemyVillainId vid)
    MinionRef mid -> goblin (EnemyMinionId mid)
    _ -> pure []
   where
    goblin enemy = do
      isGoblin <- selectAny (EnemyWithId enemy <> EnemyWithTrait Goblin)
      pure [AttackModifier 1 | isGoblin]

instance RunMessage GoblinNation where
  runMessage msg s@(GoblinNation attrs) = runQueueT $ case msg of
    -- Boost: put Goblin Nation into play
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      you <- getActivePlayerId
      s <$ putBoostIntoPlay target you
    _ -> GoblinNation <$> runMessage msg attrs
