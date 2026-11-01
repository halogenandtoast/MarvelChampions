module Marvel.Ally.Allies.Beak where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Stats
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers ()
import Marvel.Ally.Runner

beak :: AllyCard Beak
beak = ally Beak Cards.beak (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype Beak = Beak (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Beak where
  getAbilities a@(Beak attrs) = [limitedWindowAbility a 1 (W.PlayedFromHand (IdentityWithId $ allyController attrs) $ CardIs Cards.beak) Response
    (OwnsThis <> SchemeExists ThwartableScheme) NoCost (runAbility a 1)]
instance RunMessage Beak where
  runMessage msg a@(Beak attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      n <- length <$> selectList (AllyControlledBy (IdentityWithId you) <> AllyWithTrait Trait.XMen)
      removeThreat you attrs (fromIntegral n) ThwartableScheme
      pure a
    _ -> Beak <$> runMessage msg attrs
