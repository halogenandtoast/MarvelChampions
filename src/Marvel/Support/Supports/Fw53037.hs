{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Support.Supports.Fw53037 where
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
import Marvel.Support.Types

fw53037 :: SupportCard Fw53037
fw53037 = supportWith Fw53037 Cards.fw53037 ((usesL .~ 3) . (discardIfNoUsesL .~ True))

newtype Fw53037 = Fw53037 (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53037 where
  getAbilities a@(Fw53037 attrs) =
    [limitedWindowAbility a 1 (W.WindowOneOf
      [ W.DefendingCharacterWouldTakeDamage AnyCharacter
      ]) Interrupt NoCriteria UseCost (runAbility a 1)]

instance RunMessage Fw53037 where
  runMessage msg a@(Fw53037 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      for_ windows $ \case
        W.IdentityTakeDamage who _ -> preventDamage who 1
        W.AllyTakeDamage aid _ -> preventAllyDamage aid 1
        _ -> pure ()
      whenM (selectAny ThwartableScheme) $ removeThreat you attrs 1 ThwartableScheme
      pure a
    _ -> Fw53037 <$> runMessage msg attrs
