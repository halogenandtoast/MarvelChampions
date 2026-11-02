{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.Fw54017 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Upgrade.Types

fw54017 :: UpgradeCard Fw54017
fw54017 = upgrade Fw54017 Cards.fw54017

newtype Fw54017 = Fw54017 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw54017 where
  getAbilities a@(Fw54017 attrs) =
    [limitedWindowAbility a 1 (W.EngagesMinion W.After (IdentityWithId $ upgradeController attrs) AnyMinion) HeroResponse (OwnsThis) (DiscardCost $ toTarget attrs) (runAbility a 1)]

instance RunMessage Fw54017 where
  runMessage msg a@(Fw54017 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      search you (SearchIdentityDeck you AllOfDeck) (CardWithType EventType <> CardWithTrait T.Attack) SearchDrawOne ShuffleBackIn
      pure a
    _ -> Fw54017 <$> runMessage msg attrs
