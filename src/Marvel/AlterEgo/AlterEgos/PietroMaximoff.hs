module Marvel.AlterEgo.AlterEgos.PietroMaximoff (
  pietroMaximoff,
  PietroMaximoff (..),
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

pietroMaximoff :: AlterEgoCard PietroMaximoff
pietroMaximoff =
  alterEgo
    PietroMaximoff
    Cards.pietroMaximoff
    (HP $ Static 9)
    (HandSize 6)
    (Rec 3)
    [Cards.needForSpeed]

newtype PietroMaximoff = PietroMaximoff (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Superpowered Siblings — Action: Discard 2 cards from your hand → draw 2 cards (draw
-- 3 cards instead if Wanda Maximoff is in play). (Limit once per round.)
instance HasAbilities PietroMaximoff where
  getAbilities a =
    [ label "Superpowered Siblings" $
        limitedAbility a 1 (PerRound 1) Action IsSelf (DiscardHandCardsCost 2 (Just 2)) $ runAbility a 1
    ]

instance RunMessage PietroMaximoff where
  runMessage msg ae@(PietroMaximoff a) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      -- Wanda Maximoff: the Scarlet Witch ally (her subtitle), or an identity whose
      -- face-up side is titled so. ponytail: the ally by card, not by subtitle; an ally
      -- matcher by subtitle when another Wanda ally is printed
      wandaAlly <- selectAny (AllyIs Allies.scarletWitchWandaMaximoff)
      wandaIdentity <- selectAny (IdentityWithTitle "Wanda Maximoff")
      drawCards ident (if wandaAlly || wandaIdentity then 3 else 2)
      pure ae
    _ -> PietroMaximoff <$> runMessage msg a
