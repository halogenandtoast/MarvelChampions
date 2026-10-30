module Marvel.Minion.Minions.HellfirePawn where

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

hellfirePawn :: MinionCard HellfirePawn
hellfirePawn = minion HellfirePawn Cards.hellfirePawn (Sch 1) (Atk 2) (HP 3)

newtype HellfirePawn = HellfirePawn (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage HellfirePawn where
  runMessage msg e@(HellfirePawn attrs) = runQueueT $ case msg of
    Boost msg' -> case msg' of
      RevealedAsBoost target enemy | isTarget attrs target -> do
        you <- getBoostedPlayer enemy
        putBoostIntoPlay attrs you
        pure e
      _ -> pure e
    _ -> HellfirePawn <$> runMessage msg attrs
