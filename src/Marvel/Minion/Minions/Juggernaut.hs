module Marvel.Minion.Minions.Juggernaut where

import Marvel.Prelude
import Marvel.Card.Side
import Marvel.Payment
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Stats
import Marvel.Status
import Marvel.Id
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Keyword

juggernaut :: MinionCard Juggernaut
juggernaut = minion Juggernaut Cards.juggernaut (Sch 1) (Atk 4) (HP 8)

newtype Juggernaut = Juggernaut (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Juggernaut where
  runMessage msg e@(Juggernaut attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      attackGains enemy Overkill
      attackGains enemy Piercing
      pure e
    _ -> Juggernaut <$> runMessage msg attrs
