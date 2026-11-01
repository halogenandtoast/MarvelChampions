module Marvel.Minion.Minions.NextEvolutionBlackTomCassidy where

import Marvel.Minion.Runner
import Marvel.Prelude
import Marvel.Status
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability
import Marvel.Attack
import Marvel.Cost
import Marvel.Count
import Marvel.Criteria
import Marvel.Damage
import Marvel.Difficulty
import Marvel.Game.Source
import Marvel.Identity.Types
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.NextEvolution.Juggernaut
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Entity
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype BlackTomCassidyMinion = BlackTomCassidyMinion (Attrs Minion)
  deriving anyclass (IsMinion)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40132 :: MinionCard BlackTomCassidyMinion
ne40132 = minion BlackTomCassidyMinion Cards.ne40132 (Sch 1) (Atk 1) (HP 6)

ne40133 :: MinionCard BlackTomCassidyMinion
ne40133 = minion BlackTomCassidyMinion Cards.ne40133 (Sch 0) (Atk 1) (HP 3)


instance HasAbilities BlackTomCassidyMinion where
  getAbilities a@(BlackTomCassidyMinion attrs)
    | toCardCode attrs == "40133" = [windowAbility a 1
        (W.EnemyAttackedAndDamaged (EnemyWithId $ EnemyMinionId $ minionId attrs) AnyCharacter)
        ForcedResponse NoCost (runAbility a 1)]
    | otherwise = []

instance HasModifiersFor BlackTomCassidyMinion where
  getModifiersFor _ (MinionRef mid) (BlackTomCassidyMinion attrs)
    | mid == minionId attrs, toCardCode attrs == "40132" = do
        willows <- selectAny (MinionIs Cards.ne40133)
        pure [CannotTakeDamage | willows]
  getModifiersFor _ _ _ = pure []

instance RunMessage BlackTomCassidyMinion where
  runMessage msg a@(BlackTomCassidyMinion attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs, toCardCode attrs == "40132" -> do
      searchAndPutIntoPlayEncounterCard you (CardWithTitle "Creeping Willow")
      pure a
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [character | W.EnemyAttacksAndDamages _ character _ <- windows] $ \character -> giveStatus character StunnedStatus
      pure a
    Boost (RevealedAsBoost target enemy) | target == toTarget attrs, toCardCode attrs == "40133" -> do
      you <- getBoostedPlayer enemy
      giveStatus you StunnedStatus
      pure a
    _ -> BlackTomCassidyMinion <$> runMessage msg attrs
