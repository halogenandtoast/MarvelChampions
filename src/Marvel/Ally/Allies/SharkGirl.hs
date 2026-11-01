module Marvel.Ally.Allies.SharkGirl where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Stats
import Marvel.Entity
import Data.HashMap.Strict qualified as HashMap
import Marvel.Game.Source
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query (selectList)
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers ()
import Marvel.Ally.Runner

sharkGirl :: AllyCard SharkGirl
sharkGirl = ally SharkGirl Cards.sharkGirl (Thw 0, 1) (Atk 2, 1) (HP 2)

newtype SharkGirl = SharkGirl (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor SharkGirl where
  getModifiersFor _ target (SharkGirl attrs) | target == toRef attrs = do
    upgrades <- selectList (UpgradeMatches [])
    enemies <- mapMaybeM getUpgradeAttachedEnemy upgrades
    let counts = HashMap.fromListWith (+) [(enemy, 1 :: Natural) | enemy <- enemies]
    pure [AttackModifierAgainst enemy n | (enemy,n) <- HashMap.toList counts]
  getModifiersFor _ _ _ = pure []
instance RunMessage SharkGirl where
  runMessage msg (SharkGirl attrs) = runQueueT $ SharkGirl <$> runMessage msg attrs
