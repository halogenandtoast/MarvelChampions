module Marvel.Minion.Minions.Selene where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity ()
import Marvel.Game.Source
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier ()
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref ()
import Marvel.Resource.Types ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window ()
import Marvel.Identity.Types ()

import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

selene :: MinionCard Selene
selene = minion Selene Cards.selene (Sch 1) (Atk 1) (HP 4)

newtype Selene = Selene (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Selene where
  getModifiersFor _ (AllyRef _) (Selene a) = pure [CannotAttackEnemy $ EnemyMinionId $ minionId a]
  getModifiersFor _ _ _ = pure []

instance RunMessage Selene where
  runMessage msg e@(Selene attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      allies <- selectList (AllyControlledBy $ IdentityWithId you)
      chooseOne you [targeting aid $ removeFromPlay aid | aid <- allies]
      pure e
    _ -> Selene <$> runMessage msg attrs
