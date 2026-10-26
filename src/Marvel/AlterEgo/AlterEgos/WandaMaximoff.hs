module Marvel.AlterEgo.AlterEgos.WandaMaximoff (
  wandaMaximoff,
  WandaMaximoff (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Allies
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
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Stats

wandaMaximoff :: AlterEgoCard WandaMaximoff
wandaMaximoff =
  alterEgo
    WandaMaximoff
    Cards.wandaMaximoff
    (HP $ Static 10)
    (HandSize 6)
    (Rec 3)
    [Cards.slippingSanity, Cards.slippingSanity]

newtype WandaMaximoff = WandaMaximoff (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Superpowered Siblings — Action: Discard 2 cards from your hand → draw 2 cards (draw
-- 3 cards instead if Pietro Maximoff is in play). (Limit once per round.)
instance HasAbilities WandaMaximoff where
  getAbilities a =
    [ label "Superpowered Siblings" $
        limitedAbility a 1 (PerRound 1) Action IsSelf (DiscardHandCardsCost 2 (Just 2)) $ runAbility a 1
    ]

instance RunMessage WandaMaximoff where
  runMessage msg ae@(WandaMaximoff a) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      -- Pietro Maximoff: the Quicksilver ally (his subtitle), or an identity whose
      -- face-up side is titled so
      pietroAlly <- selectAny (AllyIs Allies.quicksilverPietroMaximoff)
      pietroIdentity <- selectAny (IdentityWithTitle "Pietro Maximoff")
      drawCards ident (if pietroAlly || pietroIdentity then 3 else 2)
      pure ae
    _ -> WandaMaximoff <$> runMessage msg a
