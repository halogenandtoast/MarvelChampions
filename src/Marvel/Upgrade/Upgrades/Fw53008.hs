{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.Fw53008 where
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

fw53008 :: UpgradeCard Fw53008
fw53008 = upgrade Fw53008 Cards.fw53008

newtype Fw53008 = Fw53008 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53008 where
  getAbilities a@(Fw53008 attrs) =
    [limitedWindowAbility a 1 (W.WindowOneOf
      [ W.AllyWouldTakeDamage AnyAlly
      , W.CharacterWouldTakeDamage (IdentityCharacters AnyIdentity <> CharacterOtherThan (IdentityCharacter $ upgradeController attrs))
      ]) HeroInterrupt OwnsThis (DiscardCost $ toTarget attrs) (runAbility a 1)]

instance RunMessage Fw53008 where
  runMessage msg a@(Fw53008 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      for_ windows $ \case
        W.AllyTakeDamage aid damage -> preventAllyDamage aid (damageAmount damage) >> changeToAlterEgo you
        W.IdentityTakeDamage who damage | who /= you -> preventDamage who (damageAmount damage) >> changeToAlterEgo you >> changeToAlterEgo who
        _ -> pure ()
      pure a
    _ -> Fw53008 <$> runMessage msg attrs
