module Marvel.AlterEgo.AlterEgos.RocketRaccoonAlterEgo (
  rocketRaccoonAlterEgo,
  RocketRaccoonAlterEgo (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait qualified as Trait

rocketRaccoonAlterEgo :: AlterEgoCard RocketRaccoonAlterEgo
rocketRaccoonAlterEgo =
  alterEgo
    RocketRaccoonAlterEgo
    Cards.rocketRaccoonAlterEgo
    (HP $ Static 9)
    (HandSize 6)
    (Rec 3)
    [Cards.crisisOnHalfworld]

newtype RocketRaccoonAlterEgo = RocketRaccoonAlterEgo (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Tinkering — Action: Choose and discard a Tech upgrade you control → draw 2 cards.
-- (Limit once per round.)
instance HasAbilities RocketRaccoonAlterEgo where
  getAbilities a =
    [ label "Tinkering" $
        limitedAbility a 1 (PerRound 1) Action IsSelf (DiscardUpgradeCost $ UpgradeWithTrait Trait.Tech) $
          runAbility a 1
    ]

instance RunMessage RocketRaccoonAlterEgo where
  runMessage msg ae@(RocketRaccoonAlterEgo a) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      drawCards ident 2
      pure ae
    _ -> RocketRaccoonAlterEgo <$> runMessage msg a
