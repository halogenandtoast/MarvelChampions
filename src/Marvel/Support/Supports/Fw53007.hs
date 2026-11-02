{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Support.Supports.Fw53007 where
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

import Marvel.AlterEgo.Runner (getModifiedRecovery)
import Marvel.Identity.Types (Field(PlayerIdentitySide,PlayerIdentitySides), PlayerIdentitySide(..))
import Marvel.FalconWinter.Helpers
fw53007 :: SupportCard Fw53007
fw53007 = support Fw53007 Cards.fw53007

newtype Fw53007 = Fw53007 (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53007 where
  getAbilities a@(Fw53007 attrs) =
    [ability a 1 AlterEgoAction OwnsThis (ExhaustCost <> ExhaustCharacterCost (IdentityCharacters $ IdentityWithId $ supportController attrs)) (runAbility a 1)]

instance RunMessage Fw53007 where
  runMessage msg a@(Fw53007 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      side <- project PlayerIdentitySide you
      sides <- project PlayerIdentitySides you
      case lookup side sides of
        Just (AlterEgoSide ae) -> heal you =<< getModifiedRecovery (toAttrs ae)
        _ -> pure ()
      nextCardReduction you attrs (CardMatchesAny [CardWithType AllyType, CardWithType SupportType]) 2
      pure a
    _ -> Fw53007 <$> runMessage msg attrs
