{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.SideScheme.SideSchemes.Fw54036 where
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
import Marvel.SideScheme.Types

fw54036 :: SideSchemeCard Fw54036
fw54036 = sideScheme Fw54036 Cards.fw54036 (PerPlayer 3)

newtype Fw54036 = Fw54036 (Attrs SideScheme)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme Fw54036 where
  sideSchemeAbilities = getAbilities

instance HasAbilities Fw54036 where
  getAbilities a@(Fw54036 attrs) =
    [limitedWindowAbility a 1 (W.AllyEntersPlay W.After AnyAlly) ForcedResponse (NoCriteria) (NoCost) (runAbility a 1)]

instance RunMessage Fw54036 where
  runMessage msg a@(Fw54036 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      for_ [aid | W.AllyEnteredPlay aid <- windows] exhaust
      pure a
    _ -> Fw54036 <$> runMessage msg attrs
