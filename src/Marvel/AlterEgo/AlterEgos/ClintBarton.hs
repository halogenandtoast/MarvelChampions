module Marvel.AlterEgo.AlterEgos.ClintBarton where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Card.Def
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Upgrade.Cards qualified as Upgrades

clintBarton :: AlterEgoCard ClintBarton
clintBarton =
  alterEgo
    ClintBarton
    Cards.clintBarton
    (HP $ Static 9)
    (HandSize 6)
    (Rec 3)
    [Cards.criminalPast]

newtype ClintBarton = ClintBarton (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Weapon of Choice — Action: Spend 1 resource of any type → search your deck and
-- discard pile for Hawkeye's Bow and add it to your hand. Shuffle your deck. (Limit once
-- per phase.)
instance HasAbilities ClintBarton where
  getAbilities a =
    [ label "Weapon of Choice" $
        limitedAbility a 1 (PerPhase 1) Action IsSelf (ResourceCost Nothing) $
          runAbility a 1
    ]

instance RunMessage ClintBarton where
  runMessage msg ae@(ClintBarton a) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      search ident (SearchIdentityDeckAndDiscardPile ident) (CardIs Upgrades.hawkeyesBow) SearchDrawOne ShuffleBackIn
      pure ae
    _ -> ClintBarton <$> runMessage msg a
