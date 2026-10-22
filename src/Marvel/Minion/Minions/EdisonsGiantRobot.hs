module Marvel.Minion.Minions.EdisonsGiantRobot (
  edisonsGiantRobot,
  EdisonsGiantRobot (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Resource

edisonsGiantRobot :: MinionCard EdisonsGiantRobot
edisonsGiantRobot =
  minion
    (EdisonsGiantRobot . (`With` Meta False))
    Cards.edisonsGiantRobot
    (Sch 1)
    (Atk 2)
    (HP 8)

-- | Whether its printed text box is treated as blank, until the end of the phase.
newtype Meta = Meta {blank :: Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Show, Eq)

newtype EdisonsGiantRobot = EdisonsGiantRobot (Attrs Minion `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsMinion EdisonsGiantRobot where
  toMinionAttrs (EdisonsGiantRobot (attrs `With` _)) = attrs
  overMinionAttrs f (EdisonsGiantRobot (attrs `With` meta)) = EdisonsGiantRobot (f attrs `With` meta)

instance IsRef EdisonsGiantRobot where
  toRef = toRef . toMinionAttrs

instance HasModifiersFor EdisonsGiantRobot where
  getModifiersFor _ (MinionRef mid) (EdisonsGiantRobot (attrs `With` meta))
    | mid == minionId attrs && not (blank meta) = pure [CannotTakeDamage]
  getModifiersFor _ _ _ = pure []

instance HasAbilities EdisonsGiantRobot where
  getAbilities a@(EdisonsGiantRobot (_ `With` meta)) =
    [ ability a 1 HeroAction NoCriteria (ResourceCost $ Just Mental) (runAbility a 1)
    | not (blank meta)
    ]

instance RunMessage EdisonsGiantRobot where
  runMessage msg a@(EdisonsGiantRobot (attrs `With` meta)) = case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> pure $ EdisonsGiantRobot (attrs `With` Meta True)
    EndPhase _ -> pure $ EdisonsGiantRobot (attrs `With` Meta False)
    _ -> EdisonsGiantRobot . (`With` meta) <$> runMessage msg attrs
