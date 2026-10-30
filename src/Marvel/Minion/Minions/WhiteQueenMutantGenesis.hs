module Marvel.Minion.Minions.WhiteQueenMutantGenesis where

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

whiteQueenMutantGenesis :: MinionCard WhiteQueenMutantGenesis
whiteQueenMutantGenesis = minion WhiteQueenMutantGenesis Cards.whiteQueenMutantGenesis (Sch 1) (Atk 1) (HP 5)

newtype WhiteQueenMutantGenesis = WhiteQueenMutantGenesis (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor WhiteQueenMutantGenesis where
  getModifiersFor _ (IdentityRef you) (WhiteQueenMutantGenesis a) = pure [ContinuouslyConfused | you == minionEngagedIdentity a]
  getModifiersFor _ _ _ = pure []

instance RunMessage WhiteQueenMutantGenesis where
  runMessage msg e@(WhiteQueenMutantGenesis attrs) = runQueueT $ case msg of
    Boost msg' -> case msg' of
      RevealedAsBoost target enemy | isTarget attrs target -> do
        you <- getBoostedPlayer enemy
        giveStatus you ConfusedStatus
        pure e
      _ -> pure e
    _ -> WhiteQueenMutantGenesis <$> runMessage msg attrs
