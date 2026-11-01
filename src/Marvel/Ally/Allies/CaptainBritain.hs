module Marvel.Ally.Allies.CaptainBritain where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability.Types qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

captainBritain :: AllyCard CaptainBritain
captainBritain = ally CaptainBritain Cards.captainBritain (Thw 3, 2) (Atk 3, 2) (HP 3)

newtype CaptainBritain = CaptainBritain (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor CaptainBritain where
  getModifiersFor _ target (CaptainBritain a) | isTarget a target = do
    pure [SideSchemeThwartConsequentialReduction 1, MinionAttackConsequentialDamageReduction 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage CaptainBritain where
  runMessage msg (CaptainBritain a) = runQueueT $ CaptainBritain <$> runMessage msg a
