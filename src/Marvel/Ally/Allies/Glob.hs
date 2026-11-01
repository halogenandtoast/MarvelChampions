module Marvel.Ally.Allies.Glob where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
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
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers ()
import Marvel.Ally.Runner

glob :: AllyCard Glob
glob = ally Glob Cards.glob (Thw 2, 2) (Atk 2, 1) (HP 3)

newtype Glob = Glob (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Glob where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response
    (OwnsThis <> EnemyExists (DamageableEnemy <> EnemyWithUpgrade (UpgradeMatches []))) NoCost (runAbility a 1)]
instance RunMessage Glob where
  runMessage msg a@(Glob attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ chooseDamage you attrs (toDamage 2 FromAbility) (EnemyWithUpgrade $ UpgradeMatches [])
    _ -> Glob <$> runMessage msg attrs
