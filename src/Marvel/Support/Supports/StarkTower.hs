module Marvel.Support.Supports.StarkTower (
  starkTower,
  StarkTower (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Trait

starkTower :: SupportCard StarkTower
starkTower = support StarkTower Cards.starkTower

techUpgrade :: CardMatcher
techUpgrade = CardWithTrait Tech <> CardWithType UpgradeType

newtype StarkTower = StarkTower (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities StarkTower where
  getAbilities (StarkTower a) =
    [ ability
        a
        1
        AlterEgoAction
        ( OwnsThis
            <> ExtendedCardExists
              (TopmostCardInDiscardOf AnyIdentity techUpgrade)
        )
        ExhaustCost
        $ RunAbility (toTarget a) 1
    ]

instance RunMessage StarkTower where
  runMessage msg s@(StarkTower attrs) = runQueueT $ case msg of
    -- only a player with a Tech upgrade in their discard pile is worth choosing
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      players <- getPlayers
      returns <- forMaybeM players \player ->
        fmap (player,) <$> selectOne (TopmostCardInDiscardOf (IdentityWithId player) techUpgrade)
      chooseOne ident [targeting player (addToHand player card) | (player, card) <- returns]
      pure s
    _ -> StarkTower <$> runMessage msg attrs
